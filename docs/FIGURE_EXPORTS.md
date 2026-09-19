# Colour controls and PNG figures

The single-cloud viewer uses source data, without running a classifier.
**Automatic** selects source classification if any class above 1 is present;
otherwise it uses non-zero source intensity, then source elevation. Select
Classification, Intensity or Elevation to override this choice. Missing
intensity is shown in grey with an explicit message, not invented values.
Raw intensity is sensor return strength, not calibrated reflectance and not
necessarily comparable across campaigns. Zero intensity remains visible
when intensity is explicitly selected.

The palette selector contains all 13 supported choices: Greyscale, Viridis,
Magma, Plasma, Cividis, Grey, Black, Light purple, Pale yellow, Blue, Red,
Cyan and Orange. Classification uses fixed category colours instead of a
continuous palette. Comparison panels keep their independent campaign palettes.

Camera buttons download PNGs locally:

- **Save map PNG** captures the current map extent, visible tiles, AOI outline,
  acquisition-year legend and attribution. Hide unwanted year layers first.
  Uncheck **Include basemap in image** to export outlines without imagery.
  If a provider blocks browser image capture, the app reports this instead
  of silently producing an incomplete basemap.
- **Save point-cloud PNG** captures the current angle, colours and zoom,
  with classification keys or a continuous legend, source label and display notes.
- Comparison, profile, combined and elevation-distribution figures have
  their own camera buttons. A profile must be drawn before profile exports.

Images contain displayed samples, not new analytical products. They are
created in the browser; no screenshot-upload service is used. Cite original
surveys when using a figure. Map imagery retains its provider attribution.

Map capture uses bundled html2canvas 1.4.1 (MIT); see `inst/NOTICE` and the
bundled licence. The unminified JavaScript is included in the R package,
with no CDN fetch at app runtime.
