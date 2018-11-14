module GlamorBackend: Css_Types.CssBackend_Intf = {
  type css = Css_Types.css;
  type fontFace = Css_Types.fontFace;

  [@bs.send] external className: css => string = "toString";
  [@bs.module "glamor"] external _make: Js.Json.t => css = "css";
  [@bs.scope "css"] [@bs.module "glamor"]
  external makeGlobal: (string, Js.Json.t) => unit = "global";
  [@bs.scope "css"] [@bs.module "glamor"]
  external makeInsert: string => unit = "insert";
  [@bs.scope "css"] [@bs.module "glamor"]
  external makeKeyFrames: Js.Dict.t(Js.Json.t) => string = "keyframes";
  [@bs.scope "css"] [@bs.module "glamor"]
  external makeFontFace: fontFace => string = "fontFace";
  [@bs.obj]
  external fontFace:
    (
      ~fontFamily: string,
      ~src: string,
      ~fontStyle: string=?,
      ~fontWeight: int=?
    ) =>
    fontFace =
    "";
  let merge: list(css) => css = [%bs.raw
    {|
      function (styles) {
          const glamor = require('glamor');
          return glamor.css.apply(glamor, styles)
      }
  |}
  ];
  let rec rulesetToDict = ruleset => {
    let toJs = rule =>
      switch (rule) {
      | `declaration(name, value) when name == "content" => (
          name,
          Js.Json.string(value == "" ? "\"\"" : value),
        )
      | `declaration(name, value) => (name, Js.Json.string(value))
      | `selector(name, ruleset) => (name, rulesetToDict(ruleset))
      | `shadow(value) => ("boxShadow", Js.Json.string(value))
      | `transition(value) => ("transition", Js.Json.string(value))
      | `animation(value) => ("animation", Js.Json.string(value))
      };
    ruleset |> List.map(toJs) |> Js.Dict.fromList |> Js.Json.object_;
  };
  let makeDict = ruleset => rulesetToDict(ruleset);
  let make = rules => rules |> List.rev |> makeDict |> _make;
};

/*
 module EmotionBackend: Css_Types.CssBackend_Intf = {
   /* TODO */
 };
 */

module Glamor = MakeCss(GlamorBackend);
/*module Emotion = MakeCss(EmotionBackend);*/

/* To keep backward compatibility for now, Css is equivalent to Css.Glamor */
include Glamor;
