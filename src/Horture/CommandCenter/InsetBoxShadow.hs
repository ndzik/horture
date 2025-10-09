module Horture.CommandCenter.InsetBoxShadow
  ( InsetShadowCfg (..),
    insetShadow,
    insetShadow_,
  )
where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (^.))
import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Container

-- Inset shadow config (reuse BoxShadowCfg if you prefer).
data InsetShadowCfg = InsetShadowCfg
  { iscRadius :: Maybe Double,
    iscColor :: Maybe Color
  }
  deriving (Eq, Show)

instance Default InsetShadowCfg where
  def = InsetShadowCfg Nothing Nothing

instance Semigroup InsetShadowCfg where
  a <> b =
    InsetShadowCfg
      { iscRadius = iscRadius b <|> iscRadius a,
        iscColor = iscColor b <|> iscColor a
      }

instance CmbRadius InsetShadowCfg where
  radius r =
    def
      { iscRadius = Just r
      }

instance Monoid InsetShadowCfg where mempty = InsetShadowCfg Nothing Nothing

insetShadow :: WidgetNode s e -> WidgetNode s e
insetShadow = insetShadow_ mempty

insetShadow_ :: [InsetShadowCfg] -> WidgetNode s e -> WidgetNode s e
insetShadow_ cfg child =
  defaultWidgetNode "insetShadow" (insetShadowWidget (mconcat cfg))
    & L.children
      .~ Seq.singleton child

insetShadowWidget :: InsetShadowCfg -> Widget s e
insetShadowWidget cfg = widget
  where
    widget =
      createContainer
        ()
        def
          { containerGetSizeReq = \_ _ ch -> (wReq ch, hReq ch),
            containerResize = resize,
            containerRender = render
          }

    wReq ch = maybe (fixedSize 0) (_wniSizeReqW . _wnInfo) (Seq.lookup 0 ch)
    hReq ch = maybe (fixedSize 0) (_wniSizeReqH . _wnInfo) (Seq.lookup 0 ch)

    resize _wenv node vp ch =
      ( resultNode node,
        fmap (\c -> if _wniVisible (_wnInfo c) then vp else def) ch
      )

    render wenv node renderer = do
      let style = currentStyle wenv node
          Rect l t w h = getContentArea node style
          r = min (fromMaybe 8 (iscRadius cfg)) (min (w / 2) (h / 2))
          col = fromMaybe (wenv ^. L.theme . L.basic . L.shadowColor) (iscColor cfg)
          transparent = rgba 0 0 0 0

          -- edge bands inside content rect
          topR = Rect l t w r
          botR = Rect l (t + h - r) w r
          leftR = Rect l t r h
          rightR = Rect (l + w - r) t r h

      beginPath renderer
      -- Top: fade from col at border to transparent towards center
      setFillBoxGradient renderer topR r (2 * r) col transparent
      renderRect renderer topR
      fill renderer

      -- Bottom
      beginPath renderer
      setFillBoxGradient renderer botR r (2 * r) col transparent
      renderRect renderer botR
      fill renderer

      -- Left
      beginPath renderer
      setFillBoxGradient renderer leftR r (2 * r) col transparent
      renderRect renderer leftR
      fill renderer

      -- Right
      beginPath renderer
      setFillBoxGradient renderer rightR r (2 * r) col transparent
      renderRect renderer rightR
      fill renderer
