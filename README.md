# yampa-vty-example
An example how to use yampa with vty.

**AppInput.hs** Handle Input events from vty.

**AppState.hs:** Define the whole AppState and how it is rendered.

**MainSF:** The main signal function with the type signature **SF AppInput AppState**.

**Main.hs:** Glue Yampa and vty together and set minimal FPS.
