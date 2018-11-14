type css;
type fontFace;
type rule = [
  | `declaration(string, string)
  | `selector(string, list(rule))
  | `shadow(string)
  | `transition(string)
  | `animation(string)
];

module type CssBackend_Intf = {
  let fontFace:
    (
      ~fontFamily: string,
      ~src: string,
      ~fontStyle: string=?,
      ~fontWeight: int=?
    ) =>
    fontFace;
  let makeFontFace: fontFace => string;

  let merge: list(css) => css;
  let className: css => string;

  let makeKeyFrames: Js.Dict.t(Js.Json.t) => string;
  let makeInsert: string => unit;
  let makeGlobal: (string, Js.Json.t) => unit;
  let makeDict: list(rule) => Js.Json.t;
  let make: list(rule) => css;
};

module type Css_Intf = {
  let empty: list(rule);
  let merge: list(list(rule)) => list(rule);
  let style: list(rule) => string;

  let global: (string, list(rule)) => unit;
  let insertRule: string => unit;

  let important: rule => rule;
  let label: string => rule;

  /********************************************************
   ************************ VALUES ************************
   ********************************************************/

  type cascading = [ | `inherit_ | `initial | `unset];

  let inherit_: [> | `inherit_];
  let initial: [> | `initial];
  let unset: [> | `unset];

  type angle = [ | `deg(int) | `rad(float) | `grad(float) | `turn(float)];

  let deg: int => [> | `deg(int)];
  let rad: float => [> | `rad(float)];
  let grad: float => [> | `grad(float)];
  let turn: float => [> | `turn(float)];

  type color = [
    | `rgb(int, int, int)
    | `rgba(int, int, int, float)
    | `hsl(int, int, int)
    | `hsla(int, int, int, float)
    | `transparent
    | `hex(string)
    | `currentColor
  ];

  let rgb: (int, int, int) => [> | `rgb(int, int, int)];
  let rgba: (int, int, int, float) => [> | `rgba(int, int, int, float)];
  let hsl: (int, int, int) => [> | `hsl(int, int, int)];
  let hsla: (int, int, int, float) => [> | `hsla(int, int, int, float)];
  let hex: string => [> | `hex(string)];
  let transparent: [> | `transparent];
  let currentColor: [> | `currentColor];

  type gradient = [
    | `linearGradient(angle, list((int, color)))
    | `repeatingLinearGradient(angle, list((int, color)))
    | `radialGradient(list((int, color)))
    | `repeatingRadialGradient(list((int, color)))
  ];

  let linearGradient:
    (angle, list((int, color))) =>
    [> | `linearGradient(angle, list((int, color)))];
  let repeatingLinearGradient:
    (angle, list((int, color))) =>
    [> | `repeatingLinearGradient(angle, list((int, color)))];
  let radialGradient:
    list((int, color)) => [> | `radialGradient(list((int, color)))];
  let repeatingRadialGradient:
    list((int, color)) =>
    [> | `repeatingRadialGradient(list((int, color)))];

  let aliceblue: [> | `hex(string)];
  let antiquewhite: [> | `hex(string)];
  let aqua: [> | `hex(string)];
  let aquamarine: [> | `hex(string)];
  let azure: [> | `hex(string)];
  let beige: [> | `hex(string)];
  let bisque: [> | `hex(string)];
  let black: [> | `hex(string)];
  let blanchedalmond: [> | `hex(string)];
  let blue: [> | `hex(string)];
  let blueviolet: [> | `hex(string)];
  let brown: [> | `hex(string)];
  let burlywood: [> | `hex(string)];
  let cadetblue: [> | `hex(string)];
  let chartreuse: [> | `hex(string)];
  let chocolate: [> | `hex(string)];
  let coral: [> | `hex(string)];
  let cornflowerblue: [> | `hex(string)];
  let cornsilk: [> | `hex(string)];
  let crimson: [> | `hex(string)];
  let cyan: [> | `hex(string)];
  let darkblue: [> | `hex(string)];
  let darkcyan: [> | `hex(string)];
  let darkgoldenrod: [> | `hex(string)];
  let darkgray: [> | `hex(string)];
  let darkgrey: [> | `hex(string)];
  let darkgreen: [> | `hex(string)];
  let darkkhaki: [> | `hex(string)];
  let darkmagenta: [> | `hex(string)];
  let darkolivegreen: [> | `hex(string)];
  let darkorange: [> | `hex(string)];
  let darkorchid: [> | `hex(string)];
  let darkred: [> | `hex(string)];
  let darksalmon: [> | `hex(string)];
  let darkseagreen: [> | `hex(string)];
  let darkslateblue: [> | `hex(string)];
  let darkslategray: [> | `hex(string)];
  let darkslategrey: [> | `hex(string)];
  let darkturquoise: [> | `hex(string)];
  let darkviolet: [> | `hex(string)];
  let deeppink: [> | `hex(string)];
  let deepskyblue: [> | `hex(string)];
  let dimgray: [> | `hex(string)];
  let dimgrey: [> | `hex(string)];
  let dodgerblue: [> | `hex(string)];
  let firebrick: [> | `hex(string)];
  let floralwhite: [> | `hex(string)];
  let forestgreen: [> | `hex(string)];
  let fuchsia: [> | `hex(string)];
  let gainsboro: [> | `hex(string)];
  let ghostwhite: [> | `hex(string)];
  let gold: [> | `hex(string)];
  let goldenrod: [> | `hex(string)];
  let gray: [> | `hex(string)];
  let grey: [> | `hex(string)];
  let green: [> | `hex(string)];
  let greenyellow: [> | `hex(string)];
  let honeydew: [> | `hex(string)];
  let hotpink: [> | `hex(string)];
  let indianred: [> | `hex(string)];
  let indigo: [> | `hex(string)];
  let ivory: [> | `hex(string)];
  let khaki: [> | `hex(string)];
  let lavender: [> | `hex(string)];
  let lavenderblush: [> | `hex(string)];
  let lawngreen: [> | `hex(string)];
  let lemonchiffon: [> | `hex(string)];
  let lightblue: [> | `hex(string)];
  let lightcoral: [> | `hex(string)];
  let lightcyan: [> | `hex(string)];
  let lightgoldenrodyellow: [> | `hex(string)];
  let lightgray: [> | `hex(string)];
  let lightgrey: [> | `hex(string)];
  let lightgreen: [> | `hex(string)];
  let lightpink: [> | `hex(string)];
  let lightsalmon: [> | `hex(string)];
  let lightseagreen: [> | `hex(string)];
  let lightskyblue: [> | `hex(string)];
  let lightslategray: [> | `hex(string)];
  let lightslategrey: [> | `hex(string)];
  let lightsteelblue: [> | `hex(string)];
  let lightyellow: [> | `hex(string)];
  let lime: [> | `hex(string)];
  let limegreen: [> | `hex(string)];
  let linen: [> | `hex(string)];
  let magenta: [> | `hex(string)];
  let maroon: [> | `hex(string)];
  let mediumaquamarine: [> | `hex(string)];
  let mediumblue: [> | `hex(string)];
  let mediumorchid: [> | `hex(string)];
  let mediumpurple: [> | `hex(string)];
  let mediumseagreen: [> | `hex(string)];
  let mediumslateblue: [> | `hex(string)];
  let mediumspringgreen: [> | `hex(string)];
  let mediumturquoise: [> | `hex(string)];
  let mediumvioletred: [> | `hex(string)];
  let midnightblue: [> | `hex(string)];
  let mintcream: [> | `hex(string)];
  let mistyrose: [> | `hex(string)];
  let moccasin: [> | `hex(string)];
  let navajowhite: [> | `hex(string)];
  let navy: [> | `hex(string)];
  let oldlace: [> | `hex(string)];
  let olive: [> | `hex(string)];
  let olivedrab: [> | `hex(string)];
  let orange: [> | `hex(string)];
  let orangered: [> | `hex(string)];
  let orchid: [> | `hex(string)];
  let palegoldenrod: [> | `hex(string)];
  let palegreen: [> | `hex(string)];
  let paleturquoise: [> | `hex(string)];
  let palevioletred: [> | `hex(string)];
  let papayawhip: [> | `hex(string)];
  let peachpuff: [> | `hex(string)];
  let peru: [> | `hex(string)];
  let pink: [> | `hex(string)];
  let plum: [> | `hex(string)];
  let powderblue: [> | `hex(string)];
  let purple: [> | `hex(string)];
  let rebeccapurple: [> | `hex(string)];
  let red: [> | `hex(string)];
  let rosybrown: [> | `hex(string)];
  let royalblue: [> | `hex(string)];
  let saddlebrown: [> | `hex(string)];
  let salmon: [> | `hex(string)];
  let sandybrown: [> | `hex(string)];
  let seagreen: [> | `hex(string)];
  let seashell: [> | `hex(string)];
  let sienna: [> | `hex(string)];
  let silver: [> | `hex(string)];
  let skyblue: [> | `hex(string)];
  let slateblue: [> | `hex(string)];
  let slategray: [> | `hex(string)];
  let slategrey: [> | `hex(string)];
  let snow: [> | `hex(string)];
  let springgreen: [> | `hex(string)];
  let steelblue: [> | `hex(string)];
  let tan: [> | `hex(string)];
  let teal: [> | `hex(string)];
  let thistle: [> | `hex(string)];
  let tomato: [> | `hex(string)];
  let turquoise: [> | `hex(string)];
  let violet: [> | `hex(string)];
  let wheat: [> | `hex(string)];
  let white: [> | `hex(string)];
  let whitesmoke: [> | `hex(string)];
  let yellow: [> | `hex(string)];
  let yellowgreen: [> | `hex(string)];

  type length = [
    | `calc([ | `add | `sub], length, length)
    | `ch(float)
    | `cm(float)
    | `em(float)
    | `ex(float)
    | `mm(float)
    | `percent(float)
    | `pt(int)
    | `px(int)
    | `pxFloat(float)
    | `rem(float)
    | `vh(float)
    | `vmin(float)
    | `vmax(float)
    | `vw(float)
    | `zero
  ];

  type gridLength = [ length | `fr(float) | `minContent | `maxContent];

  let ch: float => [> | `ch(float)];
  let cm: float => [> | `cm(float)];
  let em: float => [> | `em(float)];
  let ex: float => [> | `ex(float)];
  let fr: float => [> | `fr(float)];
  let mm: float => [> | `mm(float)];
  let pct: float => [> | `percent(float)];
  let pt: int => [> | `pt(int)];
  let px: int => [> | `px(int)];
  let pxFloat: float => [> | `pxFloat(float)];
  let rem: float => [> | `rem(float)];
  let vh: float => [> | `vh(float)];
  let vmax: float => [> | `vmax(float)];
  let vmin: float => [> | `vmin(float)];
  let vw: float => [> | `vw(float)];
  let zero: [> | `zero];

  module Calc: {
    let (-): (length, length) => [> length];
    let (+): (length, length) => [> length];
  };

  let size: (length, length) => [> | `size(length, length)];

  let solid: [> | `solid];
  let dotted: [> | `dotted];
  let dashed: [> | `dashed];

  let localUrl: string => [> | `localUrl(string)];
  let url: string => [> | `url(string)];

  let none: [> | `none];
  let auto: [> | `auto];
  let hidden: [> | `hidden];
  let visible: [> | `visible];
  let local: [> | `local];
  let scroll: [> | `scroll];

  let paddingBox: [> | `paddingBox];
  let borderBox: [> | `borderBox];
  let contentBox: [> | `contentBox];

  let noRepeat: [> | `noRepeat];
  let repeat: [> | `repeat];
  let repeatX: [> | `repeatX];
  let repeatY: [> | `repeatY];
  let contain: [> | `contain];
  let cover: [> | `cover];

  let row: [> | `row];
  let rowReverse: [> | `rowReverse];
  let column: [> | `column];
  let columnReverse: [> | `columnReverse];
  let wrap: [> | `wrap];
  let nowrap: [> | `nowrap];
  let wrapReverse: [> | `wrapReverse];

  let flexBox: [> | `flex];
  let grid: [> | `grid];
  let inlineGrid: [> | `inlineGrid];
  let block: [> | `block];
  let inline: [> | `inline];
  let inlineBlock: [> | `inlineBlock];
  let inlineFlex: [> | `inlineFlex];

  let absolute: [> | `absolute];
  let relative: [> | `relative];
  let static: [> | `static];
  let fixed: [> | `fixed];
  let sticky: [> | `sticky];

  let flexStart: [> | `flexStart];
  let flexEnd: [> | `flexEnd];
  let center: [> | `center];
  let stretch: [> | `stretch];
  let spaceBetween: [> | `spaceBetween];
  let spaceAround: [> | `spaceAround];
  let baseline: [> | `baseline];

  let forwards: [> | `forwards];
  let backwards: [> | `backwards];
  let both: [> | `both];
  let infinite: [> | `infinite];
  let count: int => [> | `count(int)];
  let paused: [> | `paused];
  let running: [> | `running];

  let inside: [> | `inside];
  let outside: [> | `outside];

  let translate: (length, length) => [> | `translate(length, length)];
  let translate3d:
    (length, length, length) => [> | `translate3d(length, length, length)];
  let translateX: length => [> | `translateX(length)];
  let translateY: length => [> | `translateY(length)];
  let translateZ: length => [> | `translateZ(length)];
  let scale: (float, float) => [> | `scale(float, float)];
  let scale3d: (float, float, float) => [> | `scale3d(float, float, float)];
  let scaleX: float => [> | `scaleX(float)];
  let scaleY: float => [> | `scaleY(float)];
  let scaleZ: float => [> | `scaleZ(float)];
  let rotate: angle => [> | `rotate(angle)];
  let rotate3d:
    (float, float, float, angle) =>
    [> | `rotate3d(float, float, float, angle)];
  let rotateX: angle => [> | `rotateX(angle)];
  let rotateY: angle => [> | `rotateY(angle)];
  let rotateZ: angle => [> | `rotateZ(angle)];
  let skew: (angle, angle) => [> | `skew(angle, angle)];
  let skewX: angle => [> | `skewX(angle)];
  let skewY: angle => [> | `skewY(angle)];

  let italic: [> | `italic];
  let oblique: [> | `oblique];

  let underline: [> | `underline];
  let overline: [> | `overline];
  let lineThrough: [> | `lineThrough];

  let clip: [> | `clip];
  let ellipsis: [> | `ellipsis];

  let wavy: [> | `wavy];
  let double: [> | `double];

  let uppercase: [> | `uppercase];
  let lowercase: [> | `lowercase];
  let capitalize: [> | `capitalize];

  let sub: [> | `sub];
  let super: [> | `super];
  let textTop: [> | `textTop];
  let textBottom: [> | `textBottom];
  let middle: [> | `middle];

  let normal: [> | `normal];

  let breakAll: [> | `breakAll];
  let keepAll: [> | `keepAll];
  let breakWord: [> | `breakWord];

  let reverse: [> | `reverse];
  let alternate: [> | `alternate];
  let alternateReverse: [> | `alternateReverse];

  let fill: [> | `fill];
  let content: [> | `content];
  let maxContent: [> | `maxContent];
  let minContent: [> | `minContent];
  let fitContent: [> | `fitContent];

  let all: [> | `all];
  let text: [> | `text];

  let linear: [> | `linear];
  let ease: [> | `ease];
  let easeIn: [> | `easeIn];
  let easeOut: [> | `easeOut];
  let easeInOut: [> | `easeInOut];
  let stepStart: [> | `stepStart];
  let stepEnd: [> | `stepEnd];
  let steps:
    (int, [ | `start | `end_]) => [> | `steps(int, [ | `start | `end_])];
  let cubicBesier:
    (float, float, float, float) =>
    [> | `cubicBezier(float, float, float, float)];

  let round: [> | `round];
  let miter: [> | `miter];
  let bevel: [> | `bevel];
  let butt: [> | `butt];
  let square: [> | `square];

  /********************************************************
   ******************** PROPERTIES ************************
   ********************************************************/

  let unsafe: (string, string) => rule;

  /**
 * Layout
*/

  let display:
    [
      | `flex
      | `block
      | `inline
      | `inlineBlock
      | `inlineFlex
      | `grid
      | `inlineGrid
      | `none
      | cascading
    ] =>
    rule;
  let position:
    [ | `absolute | `relative | `static | `fixed | `sticky | cascading] => rule;

  let top: length => rule;
  let bottom: length => rule;
  let left: length => rule;
  let right: length => rule;

  let flex: int => rule;
  let flexGrow: int => rule;
  let flexShrink: int => rule;
  let flexBasis:
    [
      length
      | `auto
      | `fill
      | `content
      | `maxContent
      | `minContent
      | `fitContent
    ] =>
    rule;

  let flexDirection:
    [ | `row | `column | `rowReverse | `columnReverse] => rule;
  let flexWrap: [ | `wrap | `nowrap | `wrapReverse] => rule;
  let order: int => rule;

  let gridTemplateColumns: list([ gridLength | `auto]) => rule;
  let gridTemplateRows: list([ gridLength | `auto]) => rule;
  let gridAutoRows: [ length | `auto] => rule;
  let gridAutoFlow:
    [ | `column | `row | `columnDense | `rowDense | cascading] => rule;
  let gridColumn: (int, int) => rule;
  let gridRow: (int, int) => rule;
  let gridColumnStart: int => rule;
  let gridColumnEnd: int => rule;
  let gridRowStart: int => rule;
  let gridRowEnd: int => rule;
  let gridColumnGap: length => rule;
  let gridRowGap: length => rule;
  let gridGap: length => rule;

  let width: [ length | `auto] => rule;
  let minWidth: [ length | `auto] => rule;
  let maxWidth: [ length | `auto] => rule;
  let height: [ length | `auto] => rule;
  let minHeight: [ length | `auto] => rule;
  let maxHeight: [ length | `auto] => rule;

  let margin: [ length | `auto] => rule;
  let margin2: (~v: [ length | `auto], ~h: [ length | `auto]) => rule;
  let margin3:
    (
      ~top: [ length | `auto],
      ~h: [ length | `auto],
      ~bottom: [ length | `auto]
    ) =>
    rule;
  let margin4:
    (
      ~top: [ length | `auto],
      ~right: [ length | `auto],
      ~bottom: [ length | `auto],
      ~left: [ length | `auto]
    ) =>
    rule;
  let marginLeft: [ length | `auto] => rule;
  let marginRight: [ length | `auto] => rule;
  let marginTop: [ length | `auto] => rule;
  let marginBottom: [ length | `auto] => rule;

  let padding: length => rule;
  let padding2: (~v: length, ~h: length) => rule;
  let padding3: (~top: length, ~h: length, ~bottom: length) => rule;
  let padding4:
    (~top: length, ~right: length, ~bottom: length, ~left: length) => rule;
  let paddingLeft: length => rule;
  let paddingRight: length => rule;
  let paddingTop: length => rule;
  let paddingBottom: length => rule;

  let alignContent:
    [
      | `stretch
      | `flexStart
      | `center
      | `flexEnd
      | `spaceBetween
      | `spaceAround
    ] =>
    rule;
  let alignItems:
    [ | `stretch | `flexStart | `center | `flexEnd | `baseline] => rule;
  let alignSelf:
    [ | `stretch | `flexStart | `center | `flexEnd | `baseline | `auto] => rule;
  let justifySelf: [ | `flexStart | `center | `flexEnd | `stretch] => rule;
  let justifyContent:
    [
      | `flexStart
      | `center
      | `flexEnd
      | `spaceBetween
      | `spaceAround
      | `stretch
    ] =>
    rule;

  let boxSizing: [ | `borderBox | `contentBox | cascading] => rule;

  let float: [ | `left | `right | `none] => rule;
  let clear: [ | `left | `right | `both] => rule;

  let overflow: [ | `hidden | `visible | `scroll | `auto] => rule;
  let overflowX: [ | `hidden | `visible | `scroll | `auto] => rule;
  let overflowY: [ | `hidden | `visible | `scroll | `auto] => rule;

  let zIndex: int => rule;
  let contentRule: string => rule;

  let columnCount: [ | `auto | `count(int) | cascading] => rule;

  /**
 * Style
 */
  let backfaceVisibility: [ | `visible | `hidden] => rule;
  let visibility: [ | `visible | `hidden] => rule;

  let border:
    (length, [ | `solid | `dashed | `dotted | `none], [ color]) => rule;
  let borderWidth: length => rule;
  let borderStyle: [ | `solid | `dashed | `dotted | `none] => rule;
  let borderColor: color => rule;

  let borderTop:
    (length, [ | `solid | `dashed | `dotted | `none], [ color]) => rule;
  let borderTopWidth: length => rule;
  let borderTopStyle: [ | `solid | `dashed | `dotted | `none] => rule;
  let borderTopColor: color => rule;
  let borderBottom:
    (length, [ | `solid | `dashed | `dotted | `none], [ color]) => rule;
  let borderBottomWidth: length => rule;
  let borderBottomStyle: [ | `solid | `dashed | `dotted | `none] => rule;
  let borderBottomColor: color => rule;
  let borderLeft:
    (length, [ | `solid | `dashed | `dotted | `none], [ color]) => rule;
  let borderLeftWidth: length => rule;
  let borderLeftStyle: [ | `solid | `dashed | `dotted | `none] => rule;
  let borderLeftColor: color => rule;
  let borderRight: (length, [ | `solid | `dashed | `dotted], [ color]) => rule;
  let borderRightWidth: length => rule;
  let borderRightStyle: [ | `solid | `dashed | `dotted | `none] => rule;
  let borderRightColor: color => rule;

  let borderRadius: length => rule;
  let borderTopLeftRadius: length => rule;
  let borderTopRightRadius: length => rule;
  let borderBottomLeftRadius: length => rule;
  let borderBottomRightRadius: length => rule;

  let tableLayout: [ | `auto | `fixed] => rule;
  let borderCollapse: [ | `separate | `collapse] => rule;
  let borderSpacing: length => rule;

  let boxShadow:
    (
      ~x: length=?,
      ~y: length=?,
      ~blur: length=?,
      ~spread: length=?,
      ~inset: bool=?,
      color
    ) =>
    [> | `shadow(string)];
  let boxShadows: list([ | `shadow(string)]) => rule;

  let background: [ color | `url(string) | gradient | `none] => rule;
  let backgrounds: list([ color | `url(string) | gradient | `none]) => rule;
  let backgroundColor: [ color] => rule;
  let backgroundImage: [ | `url(string) | gradient | `none] => rule;
  let backgroundAttachment: [ | `scroll | `fixed | `local] => rule;
  let backgroundClip: [ | `borderBox | `contentBox | `paddingBox] => rule;
  let backgroundOrigin: [ | `borderBox | `contentBox | `paddingBox] => rule;
  let backgroundPosition: ([ length], [ length]) => rule;
  let backgroundRepeat: [ | `repeat | `noRepeat | `repeatX | `repeatY] => rule;
  let backgroundSize:
    [ | `size(length, length) | `auto | `cover | `contain] => rule;

  let cursor:
    [
      | `auto
      | `default
      | `none
      | `contextMenu
      | `help
      | `pointer
      | `progress
      | `wait
      | `cell
      | `crosshair
      | `text
      | `verticalText
      | `alias
      | `copy
      | `move
      | `noDrop
      | `notAllowed
      | `grab
      | `grabbing
      | `allScroll
      | `colResize
      | `rowResize
      | `nResize
      | `eResize
      | `sResize
      | `wResize
      | `neResize
      | `nwResize
      | `seResize
      | `swResize
      | `ewResize
      | `nsResize
      | `neswResize
      | `nwseResize
      | `zoomIn
      | `zoomOut
    ] =>
    rule;

  let clipPath: [ | `url(string)] => rule;

  type listStyleType = [
    | `disc
    | `circle
    | `square
    | `decimal
    | `lowerAlpha
    | `upperAlpha
    | `lowerGreek
    | `lowerLatin
    | `upperLatin
    | `lowerRoman
    | `upperRoman
    | `none
  ];
  let listStyle:
    (listStyleType, [ | `inside | `outside], [ | `none | `url(string)]) =>
    rule;
  let listStyleType: listStyleType => rule;
  let listStylePosition: [ | `inside | `outside] => rule;
  let listStyleImage: [ | `none | `url(string)] => rule;

  let opacity: float => rule;

  type outlineStyle = [
    | `none
    | `hidden
    | `dotted
    | `dashed
    | `solid
    | `double
    | `groove
    | `ridge
    | `inset
    | `outset
  ];
  let outline: (length, outlineStyle, color) => rule;
  let outlineStyle: outlineStyle => rule;
  let outlineWidth: length => rule;
  let outlineColor: color => rule;
  let outlineOffset: length => rule;

  let pointerEvents: [ | `auto | `none] => rule;

  /**
 * Text
 */

  type fontStyle = [ | `italic | `normal | `oblique];

  let color: color => rule;
  let fontFamily: string => rule;
  let fontFace:
    (
      ~fontFamily: string,
      ~src: list([< | `localUrl(string) | `url(string)]),
      ~fontStyle: fontStyle=?,
      ~fontWeight: int=?,
      unit
    ) =>
    string;
  let fontSize: [ length | cascading] => rule;
  let fontVariant: [ | `normal | `smallCaps] => rule;
  let fontStyle: [ fontStyle | cascading] => rule;
  let fontWeight: int => rule;
  let letterSpacing: [ | `normal | length] => rule;
  let lineHeight: [ | `normal | `abs(float) | length | cascading] => rule;
  let textAlign: [ | `left | `center | `right | `justify] => rule;
  let textDecoration:
    [ | `none | `underline | `overline | `lineThrough] => rule;
  let textDecorationColor: color => rule;
  let textDecorationStyle:
    [ | `wavy | `solid | `dotted | `dashed | `double] => rule;
  let textIndent: length => rule;
  let textOverflow: [ | `clip | `ellipsis | `string(string)] => rule;
  let textShadow: (~x: length=?, ~y: length=?, ~blur: length=?, color) => rule;
  let textTransform:
    [ | `uppercase | `lowercase | `capitalize | `none] => rule;
  let userSelect: [ | `auto | `all | `text | `none] => rule;
  let verticalAlign:
    [
      | `baseline
      | length
      | `sub
      | `super
      | `top
      | `textTop
      | `middle
      | `bottom
      | `textBottom
    ] =>
    rule;
  let whiteSpace: [ | `normal | `nowrap | `pre | `preLine | `preWrap] => rule;
  let wordBreak: [ | `breakAll | `keepAll | `normal] => rule;
  let wordSpacing: [ | `normal | length] => rule;
  let wordWrap: [ | `normal | `breakWord] => rule;

  /**
 * Transform
 */

  type transform = [
    | `translate(length, length)
    | `translate3d(length, length, length)
    | `translateX(length)
    | `translateY(length)
    | `translateZ(length)
    | `scale(float, float)
    | `scale3d(float, float, float)
    | `scaleX(float)
    | `scaleY(float)
    | `scaleZ(float)
    | `rotate(angle)
    | `rotate3d(float, float, float, angle)
    | `rotateX(angle)
    | `rotateY(angle)
    | `rotateZ(angle)
    | `skew(angle, angle)
    | `skewX(angle)
    | `skewY(angle)
    | `perspective(int)
  ];

  let transform: transform => rule;
  let transforms: list(transform) => rule;
  let transformOrigin: (length, length) => rule;
  let transformOrigin3d: (length, length, length) => rule;
  let transformStyle: [ | `preserve3d | `flat] => rule;
  let perspective: [ | `none | length] => rule;
  let perspectiveOrigin: (length, length) => rule;

  /**
  * Transition
  */
  type timingFunction = [
    | `linear
    | `ease
    | `easeIn
    | `easeOut
    | `easeInOut
    | `stepStart
    | `stepEnd
    | `steps(int, [ | `start | `end_])
    | `cubicBezier(float, float, float, float)
  ];
  let transition:
    (
      ~duration: int=?,
      ~delay: int=?,
      ~timingFunction: timingFunction=?,
      string
    ) =>
    [> | `transition(string)];
  let transitions: list([ | `transition(string)]) => rule;
  let transitionDelay: int => rule;
  let transitionDuration: int => rule;
  let transitionTimingFunction: timingFunction => rule;
  let transitionProperty: string => rule;

  /**
 * Animation
 */

  type animation;
  let keyframes: list((int, list(rule))) => animation;

  type animationDirection = [
    | `normal
    | `reverse
    | `alternate
    | `alternateReverse
  ];

  type animationFillMode = [ | `none | `forwards | `backwards | `both];
  type animationIterationCount = [ | `infinite | `count(int)];
  type animationPlayState = [ | `paused | `running];

  let animation:
    (
      ~duration: int=?,
      ~delay: int=?,
      ~direction: animationDirection=?,
      ~timingFunction: timingFunction=?,
      ~fillMode: animationFillMode=?,
      ~playState: animationPlayState=?,
      ~iterationCount: animationIterationCount=?,
      animation
    ) =>
    [> | `animation(string)];
  let animations: list([ | `animation(string)]) => rule;

  let animationDelay: int => rule;
  let animationDirection: animationDirection => rule;
  let animationDuration: int => rule;
  let animationFillMode: animationFillMode => rule;
  let animationIterationCount: [ | `infinite | `count(int)] => rule;
  let animationName: animation => rule;
  let animationPlayState: [ | `paused | `running] => rule;
  let animationTimingFunction: timingFunction => rule;

  /**
 * selectors
 */
  let selector: (string, list(rule)) => rule;
  let active: list(rule) => rule;
  let after: list(rule) => rule;
  let before: list(rule) => rule;
  let checked: list(rule) => rule;
  let children: list(rule) => rule;
  let directSibling: list(rule) => rule;
  let disabled: list(rule) => rule;
  let firstChild: list(rule) => rule;
  let firstOfType: list(rule) => rule;
  let focus: list(rule) => rule;
  let hover: list(rule) => rule;
  let lastChild: list(rule) => rule;
  let lastOfType: list(rule) => rule;
  let link: list(rule) => rule;
  let readOnly: list(rule) => rule;
  let required: list(rule) => rule;
  let visited: list(rule) => rule;
  let enabled: list(rule) => rule;
  let noContent: list(rule) => rule;
  let default: list(rule) => rule;
  let anyLink: list(rule) => rule;
  let onlyChild: list(rule) => rule;
  let onlyOfType: list(rule) => rule;
  let optional: list(rule) => rule;
  let invalid: list(rule) => rule;
  let outOfRange: list(rule) => rule;
  let siblings: list(rule) => rule;
  let target: list(rule) => rule;
  let firstLine: list(rule) => rule;
  let firstLetter: list(rule) => rule;
  let selection: list(rule) => rule;
  let placeholder: list(rule) => rule;

  let media: (string, list(rule)) => rule;

  /**
 * SVG
 */

  module SVG: {
    let fill: color => rule;
    let fillRule: [ | `nonzero | `evenodd] => rule;
    let fillOpacity: float => rule;
    let stroke: color => rule;
    let strokeLinecap: [ | `butt | `round | `square] => rule;
    let strokeLinejoin: [ | `miter | `round | `bevel] => rule;
    let strokeMiterlimit: float => rule;
    let strokeWidth: length => rule;
    let strokeOpacity: float => rule;
    let stopColor: color => rule;
    let stopOpacity: float => rule;
  };
};

module MakeCss = (CssBackend: Css_Types.CssBackend_Intf) : Css_Types.Css_Intf => {
  include Css_Colors;

  let join = (separator, strings) => {
    let rec run = (acc, strings) =>
      switch (strings) {
      | [] => acc
      | [x] => acc ++ x
      | [x, ...xs] => run(acc ++ x ++ separator, xs)
      };
    run("", strings);
  };

  let string_of_float = (f: float) => {j|$(f)|j};

  module Converter = {
    let string_of_angle =
      fun
      | `deg(x) => string_of_int(x) ++ "deg"
      | `rad(x) => string_of_float(x) ++ "rad"
      | `grad(x) => string_of_float(x) ++ "grad"
      | `turn(x) => string_of_float(x) ++ "turn";

    let string_of_rgb = (r, g, b) =>
      "rgb("
      ++ string_of_int(r)
      ++ ", "
      ++ string_of_int(g)
      ++ ", "
      ++ string_of_int(b)
      ++ ")";

    let string_of_rgba = (r, g, b, a) =>
      "rgba("
      ++ string_of_int(r)
      ++ ", "
      ++ string_of_int(g)
      ++ ", "
      ++ string_of_int(b)
      ++ ", "
      ++ string_of_float(a)
      ++ ")";

    let string_of_hsl = (h, s, l) =>
      "hsl("
      ++ string_of_int(h)
      ++ ", "
      ++ string_of_int(s)
      ++ "%, "
      ++ string_of_int(l)
      ++ "%"
      ++ ")";

    let string_of_hsla = (h, s, l, a) =>
      "hsla("
      ++ string_of_int(h)
      ++ ", "
      ++ string_of_int(s)
      ++ "%, "
      ++ string_of_int(l)
      ++ "%, "
      ++ string_of_float(a)
      ++ ")";

    let string_of_color =
      fun
      | `rgb(r, g, b) => string_of_rgb(r, g, b)
      | `rgba(r, g, b, a) => string_of_rgba(r, g, b, a)
      | `hsl(h, s, l) => string_of_hsl(h, s, l)
      | `hsla(h, s, l, a) => string_of_hsla(h, s, l, a)
      | `hex(s) => "#" ++ s
      | `transparent => "transparent"
      | `currentColor => "currentColor";

    let string_of_stops = stops =>
      stops
      |> List.map(((i, c)) =>
           join(" ", [string_of_color(c), string_of_int(i) ++ "%"])
         )
      |> join(", ");

    let string_of_linearGradient = (angle, stops) =>
      "linear-gradient("
      ++ string_of_angle(angle)
      ++ ", "
      ++ string_of_stops(stops)
      ++ ")";

    let string_of_repeatingLinearGradient = (angle, stops) =>
      "repeating-linear-gradient("
      ++ string_of_angle(angle)
      ++ ", "
      ++ string_of_stops(stops)
      ++ ")";

    let rec string_of_length =
      fun
      | `calc(`add, a, b) =>
        "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
      | `calc(`sub, a, b) =>
        "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
      | `ch(x) => string_of_float(x) ++ "ch"
      | `cm(x) => string_of_float(x) ++ "cm"
      | `em(x) => string_of_float(x) ++ "em"
      | `ex(x) => string_of_float(x) ++ "ex"
      | `mm(x) => string_of_float(x) ++ "mm"
      | `percent(x) => string_of_float(x) ++ "%"
      | `pt(x) => string_of_int(x) ++ "pt"
      | `px(x) => string_of_int(x) ++ "px"
      | `pxFloat(x) => string_of_float(x) ++ "px"
      | `rem(x) => string_of_float(x) ++ "rem"
      | `vh(x) => string_of_float(x) ++ "vh"
      | `vmax(x) => string_of_float(x) ++ "vmax"
      | `vmin(x) => string_of_float(x) ++ "vmin"
      | `vw(x) => string_of_float(x) ++ "vw"
      | `zero => "0";

    let string_of_translate3d = (x, y, z) =>
      "translate3d("
      ++ string_of_length(x)
      ++ ", "
      ++ string_of_length(y)
      ++ ", "
      ++ string_of_length(z)
      ++ ")";

    let string_of_scale = (x, y) =>
      "scale(" ++ string_of_float(x) ++ ", " ++ string_of_float(y) ++ ")";

    let string_of_time = t => string_of_int(t) ++ "ms";

    let string_of_overflow =
      fun
      | `auto => "auto"
      | `scroll => "scroll"
      | `hidden => "hidden"
      | `visible => "visible";

    let string_of_visibility =
      fun
      | `hidden => "hidden"
      | `visible => "visible";

    let string_of_background = bg =>
      switch (bg) {
      | `none => "none"
      | `url(url) => "url(" ++ url ++ ")"
      | `rgb(r, g, b) =>
        "rgb("
        ++ join(
             ", ",
             [string_of_int(r), string_of_int(g), string_of_int(b)],
           )
        ++ ")"
      | `rgba(r, g, b, a) =>
        "rgba("
        ++ join(
             ", ",
             [
               string_of_int(r),
               string_of_int(g),
               string_of_int(b),
               string_of_float(a),
             ],
           )
        ++ ")"
      | `hsl(h, s, l) =>
        "hsl("
        ++ join(
             ", ",
             [
               string_of_int(h),
               string_of_int(s) ++ "%",
               string_of_int(l) ++ "%",
             ],
           )
        ++ ")"
      | `hsla(h, s, l, a) =>
        "hsla("
        ++ join(
             ", ",
             [
               string_of_int(h),
               string_of_int(s) ++ "%",
               string_of_int(l) ++ "%",
               string_of_float(a),
             ],
           )
        ++ ")"
      | `hex(s) => "#" ++ s
      | `transparent => "transparent"
      | `currentColor => "currentColor"
      | `linearGradient(angle, stops) =>
        "linear-gradient("
        ++ string_of_angle(angle)
        ++ ", "
        ++ string_of_stops(stops)
        ++ ")"
      | `repeatingLinearGradient(angle, stops) =>
        "repeating-linear-gradient("
        ++ string_of_angle(angle)
        ++ ", "
        ++ string_of_stops(stops)
        ++ ")"
      | `radialGradient(stops) =>
        "radial-gradient(" ++ string_of_stops(stops) ++ ")"
      | `repeatingRadialGradient(stops) =>
        "repeating-radial-gradient(" ++ string_of_stops(stops) ++ ")"
      };

    let string_of_cursor = x =>
      switch (x) {
      | `auto => "auto"
      | `default => "default"
      | `none => "none"
      | `contextMenu => "context-menu"
      | `help => "help"
      | `pointer => "pointer"
      | `progress => "progress"
      | `wait => "wait"
      | `cell => "cell"
      | `crosshair => "crosshair"
      | `text => "text"
      | `verticalText => "vertical-text"
      | `alias => "alias"
      | `copy => "copy"
      | `move => "move"
      | `noDrop => "no-drop"
      | `notAllowed => "not-allowed"
      | `grab => "grab"
      | `grabbing => "grabbing"
      | `allScroll => "all-scroll"
      | `colResize => "col-resize"
      | `rowResize => "row-resize"
      | `nResize => "n-resize"
      | `eResize => "e-resize"
      | `sResize => "s-resize"
      | `wResize => "w-resize"
      | `neResize => "ne-resize"
      | `nwResize => "nw-resize"
      | `seResize => "se-resize"
      | `swResize => "sw-resize"
      | `ewResize => "ew-resize"
      | `nsResize => "ns-resize"
      | `neswResize => "nesw-resize"
      | `nwseResize => "nwse-resize"
      | `zoomIn => "zoom-in"
      | `zoomOut => "zoom-out"
      };

    let string_of_gridDirection =
      fun
      | `column => "column"
      | `row => "row"
      | `columnDense => "column dense"
      | `rowDense => "row dense"
      | `inherit_ => "inherit"
      | `initial => "initial"
      | `unset => "unset";
  };
  include Converter;

  type rule = Css_Types.rule;
  type animation = string;

  let empty = [];
  let merge = List.concat;
  let global = (selector, rules) =>
    CssBackend.makeGlobal(selector, CssBackend.makeDict(rules));
  let insertRule = css => CssBackend.makeInsert(css);

  let addStop = (dict, (stop, rules)) => {
    Js.Dict.set(
      dict,
      string_of_int(stop) ++ "%",
      CssBackend.makeDict(rules),
    );
    dict;
  };

  let keyframes = frames =>
    CssBackend.makeKeyFrames @@
    List.fold_left(addStop, Js.Dict.empty(), frames);

  let style = rules => rules |> CssBackend.make |> CssBackend.className;

  let d = (property, value) => `declaration((property, value));

  let important: rule => rule =
    v =>
      switch (v) {
      | `declaration(name, value) =>
        `declaration((name, value ++ " !important"))
      | _ => v
      };

  let label = label => `declaration(("label", label));

  /********************************************************
   ************************ VALUES ************************
   ********************************************************/
  type cascading = [ | `inherit_ | `initial | `unset];

  let inherit_ = `inherit_;
  let initial = `initial;
  let unset = `unset;

  type angle = [ | `deg(int) | `rad(float) | `grad(float) | `turn(float)];

  let deg = x => `deg(x);
  let rad = x => `rad(x);
  let grad = x => `grad(x);
  let turn = x => `turn(x);

  type color = [
    | `rgb(int, int, int)
    | `rgba(int, int, int, float)
    | `hsl(int, int, int)
    | `hsla(int, int, int, float)
    | `hex(string)
    | `transparent
    | `currentColor
  ];

  let rgb = (r, g, b) => `rgb((r, g, b));
  let rgba = (r, g, b, a) => `rgba((r, g, b, a));
  let hsl = (h, s, l) => `hsl((h, s, l));
  let hsla = (h, s, l, a) => `hsla((h, s, l, a));
  let hex = x => `hex(x);

  let currentColor = `currentColor;

  type gradient = [
    | `linearGradient(angle, list((int, color)))
    | `repeatingLinearGradient(angle, list((int, color)))
    | `radialGradient(list((int, color)))
    | `repeatingRadialGradient(list((int, color)))
  ];

  let linearGradient = (angle, stops) => `linearGradient((angle, stops));

  let repeatingLinearGradient = (angle, stops) =>
    `repeatingLinearGradient((angle, stops));

  let radialGradient = stops => `radialGradient(stops);

  let repeatingRadialGradient = stops => `repeatingRadialGradient(stops);

  /**
 * Units
 */

  type length = [
    | `calc([ | `add | `sub], length, length)
    | `ch(float)
    | `cm(float)
    | `em(float)
    | `ex(float)
    | `mm(float)
    | `percent(float)
    | `pt(int)
    | `px(int)
    | `pxFloat(float)
    | `rem(float)
    | `vh(float)
    | `vmax(float)
    | `vmin(float)
    | `vw(float)
    | `zero
  ];

  type gridLength = [ length | `fr(float) | `minContent | `maxContent];

  let string_of_length_cascading =
    fun
    | `calc(`add, a, b) =>
      "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
    | `calc(`sub, a, b) =>
      "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
    | `ch(x) => string_of_float(x) ++ "ch"
    | `cm(x) => string_of_float(x) ++ "cm"
    | `em(x) => string_of_float(x) ++ "em"
    | `ex(x) => string_of_float(x) ++ "ex"
    | `mm(x) => string_of_float(x) ++ "mm"
    | `percent(x) => string_of_float(x) ++ "%"
    | `pt(x) => string_of_int(x) ++ "pt"
    | `px(x) => string_of_int(x) ++ "px"
    | `pxFloat(x) => string_of_float(x) ++ "px"
    | `rem(x) => string_of_float(x) ++ "rem"
    | `vh(x) => string_of_float(x) ++ "vh"
    | `vmax(x) => string_of_float(x) ++ "vmax"
    | `vmin(x) => string_of_float(x) ++ "vmin"
    | `vw(x) => string_of_float(x) ++ "vw"
    | `zero => "0"
    | `inherit_ => "inherit"
    | `initial => "initial"
    | `unset => "unset";

  let ch = x => `ch(x);
  let cm = x => `cm(x);
  let em = x => `em(x);
  let ex = x => `ex(x);
  let fr = x => `fr(x);
  let mm = x => `mm(x);
  let pct = x => `percent(x);
  let pt = x => `pt(x);
  let px = x => `px(x);
  let pxFloat = x => `pxFloat(x);
  let rem = x => `rem(x);
  let vh = x => `vh(x);
  let vmax = x => `vmax(x);
  let vmin = x => `vmin(x);
  let vw = x => `vw(x);
  let zero = `zero;

  module Calc = {
    let (-) = (a, b) => `calc((`sub, a, b));
    let (+) = (a, b) => `calc((`add, a, b));
  };
  let size = (x, y) => `size((x, y));

  /**
   * Misc
   */

  let absolute = `absolute;
  let all = `all;
  let auto = `auto;
  let backwards = `backwards;
  let baseline = `baseline;
  let block = `block;
  let borderBox = `borderBox;
  let both = `both;
  let center = `center;
  let column = `column;
  let columnReverse = `columnReverse;
  let contain = `contain;
  let contentBox = `contentBox;
  let count = x => `count(x);
  let cover = `cover;
  let cubicBesier = (a, b, c, d) => `cubicBezier((a, b, c, d));
  let dashed = `dashed;
  let dotted = `dotted;
  let ease = `ease;
  let easeIn = `easeIn;
  let easeInOut = `easeInOut;
  let easeOut = `easeOut;
  let fixed = `fixed;
  let flexBox = `flex;
  let grid = `grid;
  let inlineGrid = `inlineGrid;
  let flexEnd = `flexEnd;
  let flexStart = `flexStart;
  let forwards = `forwards;
  let hidden = `hidden;
  let infinite = `infinite;
  let inline = `inline;
  let inlineBlock = `inlineBlock;
  let inlineFlex = `inlineFlex;
  let linear = `linear;
  let local = `local;
  let localUrl = x => `localUrl(x);
  let none = `none;
  let noRepeat = `noRepeat;
  let nowrap = `nowrap;
  let paddingBox = `paddingBox;
  let paused = `paused;
  let relative = `relative;
  let repeat = `repeat;
  let repeatX = `repeatX;
  let repeatY = `repeatY;
  let rotate = a => `rotate(a);
  let rotate3d = (x, y, z, a) => `rotate3d((x, y, z, a));
  let rotateX = a => `rotateX(a);
  let rotateY = a => `rotateY(a);
  let rotateZ = a => `rotateZ(a);
  let row = `row;
  let rowReverse = `rowReverse;
  let running = `running;
  let scale = (x, y) => `scale((x, y));
  let scale3d = (x, y, z) => `scale3d((x, y, z));
  let scaleX = x => `scaleX(x);
  let scaleY = x => `scaleY(x);
  let scaleZ = x => `scaleZ(x);
  let scroll = `scroll;
  let skew = (x, y) => `skew((x, y));
  let skewX = a => `skewX(a);
  let skewY = a => `skewY(a);
  let solid = `solid;
  let spaceAround = `spaceAround;
  let spaceBetween = `spaceBetween;
  let static = `static;
  let stepEnd = `stepEnd;
  let steps = (i, dir) => `steps((i, dir));
  let stepStart = `stepStart;
  let sticky = `sticky;
  let stretch = `stretch;
  let text = `text;
  let translate = (x, y) => `translate((x, y));
  let translate3d = (x, y, z) => `translate3d((x, y, z));
  let translateX = x => `translateX(x);
  let translateY = y => `translateY(y);
  let translateZ = z => `translateZ(z);
  let url = x => `url(x);
  let visible = `visible;
  let wrap = `wrap;
  let wrapReverse = `wrapReverse;

  let inside = `inside;
  let outside = `outside;

  let italic = `italic;
  let oblique = `oblique;

  let underline = `underline;
  let overline = `overline;
  let lineThrough = `lineThrough;
  let clip = `clip;
  let ellipsis = `ellipsis;

  let wavy = `wavy;
  let double = `double;

  let uppercase = `uppercase;
  let lowercase = `lowercase;
  let capitalize = `capitalize;

  let sub = `sub;
  let super = `super;
  let textTop = `textTop;
  let textBottom = `textBottom;
  let middle = `middle;

  let normal = `normal;

  let breakAll = `breakAll;
  let keepAll = `keepAll;
  let breakWord = `breakWord;

  let reverse = `reverse;
  let alternate = `alternate;
  let alternateReverse = `alternateReverse;

  let fill = `fill;
  let content = `content;
  let maxContent = `maxContent;
  let minContent = `minContent;
  let fitContent = `fitContent;

  let round = `round;
  let miter = `miter;
  let bevel = `bevel;
  let butt = `butt;
  let square = `square;

  /********************************************************
   ******************** PROPERTIES ************************
   ********************************************************/

  let unsafe = d;

  /**
   * Layout
   */

  let display = x =>
    d(
      "display",
      switch (x) {
      | `block => "block"
      | `inline => "inline"
      | `inlineBlock => "inline-block"
      | `flex => "flex"
      | `inlineFlex => "inline-flex"
      | `grid => "grid"
      | `inlineGrid => "inline-grid"
      | `none => "none"
      | `inherit_ => "inherit"
      | `initial => "initial"
      | `unset => "unset"
      },
    );

  let position = x =>
    d(
      "position",
      switch (x) {
      | `absolute => "absolute"
      | `static => "static"
      | `fixed => "fixed"
      | `relative => "relative"
      | `sticky => "sticky"
      | `inherit_ => "inherit"
      | `initial => "initial"
      | `unset => "unset"
      },
    );

  let top = x => d("top", string_of_length(x));
  let bottom = x => d("bottom", string_of_length(x));
  let left = x => d("left", string_of_length(x));
  let right = x => d("right", string_of_length(x));

  let flex = x => d("flex", string_of_int(x));
  let flexGrow = x => d("flexGrow", string_of_int(x));
  let flexShrink = x => d("flexShrink", string_of_int(x));
  let flexBasis = x =>
    d(
      "flexBasis",
      switch (x) {
      | `calc(`add, a, b) =>
        "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
      | `calc(`sub, a, b) =>
        "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
      | `ch(x) => string_of_float(x) ++ "ch"
      | `cm(x) => string_of_float(x) ++ "cm"
      | `em(x) => string_of_float(x) ++ "em"
      | `ex(x) => string_of_float(x) ++ "ex"
      | `mm(x) => string_of_float(x) ++ "mm"
      | `percent(x) => string_of_float(x) ++ "%"
      | `pt(x) => string_of_int(x) ++ "pt"
      | `px(x) => string_of_int(x) ++ "px"
      | `pxFloat(x) => string_of_float(x) ++ "px"
      | `rem(x) => string_of_float(x) ++ "rem"
      | `vh(x) => string_of_float(x) ++ "vh"
      | `vmax(x) => string_of_float(x) ++ "vmax"
      | `vmin(x) => string_of_float(x) ++ "vmin"
      | `vw(x) => string_of_float(x) ++ "vw"
      | `zero => "0"
      | `auto => "auto"
      | `fill => "fill"
      | `content => "content"
      | `maxContent => "max-content"
      | `minContent => "min-content"
      | `fitContent => "fit-content"
      },
    );

  let flexDirection = x =>
    d(
      "flexDirection",
      switch (x) {
      | `row => "row"
      | `column => "column"
      | `rowReverse => "row-reverse"
      | `columnReverse => "column-reverse"
      },
    );

  let flexWrap = x =>
    d(
      "flexWrap",
      switch (x) {
      | `nowrap => "nowrap"
      | `wrap => "wrap"
      | `wrapReverse => "wrap-reverse"
      },
    );

  let order = x => d("order", string_of_int(x));

  let string_of_margin =
    fun
    | `calc(`add, a, b) =>
      "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
    | `calc(`sub, a, b) =>
      "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
    | `ch(x) => string_of_float(x) ++ "ch"
    | `cm(x) => string_of_float(x) ++ "cm"
    | `em(x) => string_of_float(x) ++ "em"
    | `ex(x) => string_of_float(x) ++ "ex"
    | `mm(x) => string_of_float(x) ++ "mm"
    | `percent(x) => string_of_float(x) ++ "%"
    | `pt(x) => string_of_int(x) ++ "pt"
    | `px(x) => string_of_int(x) ++ "px"
    | `pxFloat(x) => string_of_float(x) ++ "px"
    | `rem(x) => string_of_float(x) ++ "rem"
    | `vh(x) => string_of_float(x) ++ "vh"
    | `vmax(x) => string_of_float(x) ++ "vmax"
    | `vmin(x) => string_of_float(x) ++ "vmin"
    | `vw(x) => string_of_float(x) ++ "vw"
    | `zero => "0"
    | `auto => "auto";

  let margin = x => d("margin", string_of_margin(x));
  let margin2 = (~v, ~h) =>
    d("margin", string_of_margin(v) ++ " " ++ string_of_margin(h));
  let margin3 = (~top, ~h, ~bottom) =>
    d(
      "margin",
      string_of_margin(top)
      ++ " "
      ++ string_of_margin(h)
      ++ " "
      ++ string_of_margin(bottom),
    );
  let margin4 = (~top, ~right, ~bottom, ~left) =>
    d(
      "margin",
      string_of_margin(top)
      ++ " "
      ++ string_of_margin(right)
      ++ " "
      ++ string_of_margin(bottom)
      ++ " "
      ++ string_of_margin(left),
    );
  let marginLeft = x => d("marginLeft", string_of_margin(x));
  let marginRight = x => d("marginRight", string_of_margin(x));
  let marginTop = x => d("marginTop", string_of_margin(x));
  let marginBottom = x => d("marginBottom", string_of_margin(x));

  let padding = x => d("padding", string_of_length(x));
  let padding2 = (~v, ~h) =>
    d("padding", string_of_length(v) ++ " " ++ string_of_length(h));
  let padding3 = (~top, ~h, ~bottom) =>
    d(
      "padding",
      string_of_length(top)
      ++ " "
      ++ string_of_length(h)
      ++ " "
      ++ string_of_length(bottom),
    );
  let padding4 = (~top, ~right, ~bottom, ~left) =>
    d(
      "padding",
      string_of_length(top)
      ++ " "
      ++ string_of_length(right)
      ++ " "
      ++ string_of_length(bottom)
      ++ " "
      ++ string_of_length(left),
    );
  let paddingLeft = x => d("paddingLeft", string_of_length(x));
  let paddingRight = x => d("paddingRight", string_of_length(x));
  let paddingTop = x => d("paddingTop", string_of_length(x));
  let paddingBottom = x => d("paddingBottom", string_of_length(x));

  let string_of_dimension =
    fun
    | `auto => "auto"
    | `calc(`add, a, b) =>
      "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
    | `calc(`sub, a, b) =>
      "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
    | `ch(x) => string_of_float(x) ++ "ch"
    | `cm(x) => string_of_float(x) ++ "cm"
    | `em(x) => string_of_float(x) ++ "em"
    | `ex(x) => string_of_float(x) ++ "ex"
    | `mm(x) => string_of_float(x) ++ "mm"
    | `percent(x) => string_of_float(x) ++ "%"
    | `pt(x) => string_of_int(x) ++ "pt"
    | `px(x) => string_of_int(x) ++ "px"
    | `pxFloat(x) => string_of_float(x) ++ "px"
    | `rem(x) => string_of_float(x) ++ "rem"
    | `vh(x) => string_of_float(x) ++ "vh"
    | `vmax(x) => string_of_float(x) ++ "vmax"
    | `vmin(x) => string_of_float(x) ++ "vmin"
    | `vw(x) => string_of_float(x) ++ "vw"
    | `fr(x) => string_of_float(x) ++ "fr"
    | `zero => "0"
    | `minContent => "min-content"
    | `maxContent => "max-content";

  let width = x => d("width", string_of_dimension(x));
  let maxWidth = x => d("maxWidth", string_of_dimension(x));
  let minWidth = x => d("minWidth", string_of_dimension(x));
  let height = x => d("height", string_of_dimension(x));
  let minHeight = x => d("minHeight", string_of_dimension(x));
  let maxHeight = x => d("maxHeight", string_of_dimension(x));

  type gridAutoDirection = [
    | `column
    | `row
    | `columnDense
    | `rowDense
    | cascading
  ];

  let gridAutoFlow = direction =>
    d("gridAutoFlow", string_of_gridDirection(direction));

  let string_of_dimensions = dimensions =>
    dimensions |> List.map(string_of_dimension) |> String.concat(" ");

  let gridTemplateColumns = dimensions =>
    d("gridTemplateColumns", string_of_dimensions(dimensions));

  let gridTemplateRows = dimensions =>
    d("gridTemplateRows", string_of_dimensions(dimensions));

  let gridAutoRows = dimensions =>
    d("gridAutoRows", string_of_dimension(dimensions));

  let gridColumn = (start, end') =>
    d("gridColumn", string_of_int(start) ++ " / " ++ string_of_int(end'));

  let gridRow = (start, end') =>
    d("gridRow", string_of_int(start) ++ " / " ++ string_of_int(end'));
  let gridColumnStart = n => d("gridColumnStart", string_of_int(n));
  let gridColumnEnd = n => d("gridColumnEnd", string_of_int(n));
  let gridRowStart = n => d("gridRowStart", string_of_int(n));
  let gridRowEnd = n => d("gridRowEnd", string_of_int(n));
  let gridColumnGap = n => d("gridColumnGap", string_of_length(n));
  let gridRowGap = n => d("gridRowGap", string_of_length(n));
  let gridGap = n => d("gridGap", string_of_length(n));

  let string_of_align =
    fun
    | `baseline => "baseline"
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    | `center => "center"
    | `auto => "auto"
    | `stretch => "stretch";
  let alignItems = x => d("alignItems", string_of_align(x));
  let alignSelf = x => d("alignSelf", string_of_align(x));

  let string_of_justify =
    fun
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    | `center => "center"
    | `spaceAround => "space-around"
    | `spaceBetween => "space-between"
    | `stretch => "stretch";
  let justifyContent = x => d("justifyContent", string_of_justify(x));
  let justifySelf = x => d("justifySelf", string_of_justify(x));
  let alignContent = x => d("alignContent", string_of_justify(x));

  let boxSizing = x =>
    d(
      "boxSizing",
      switch (x) {
      | `contentBox => "content-box"
      | `borderBox => "border-box"
      | `inherit_ => "inherit"
      | `initial => "initial"
      | `unset => "unset"
      },
    );

  let float = x =>
    d(
      "float",
      switch (x) {
      | `left => "left"
      | `right => "right"
      | `none => "none"
      },
    );

  let clear = x =>
    d(
      "clear",
      switch (x) {
      | `left => "left"
      | `right => "right"
      | `both => "both"
      },
    );

  let overflow = x => d("overflow", string_of_overflow(x));
  let overflowX = x => d("overflowX", string_of_overflow(x));
  let overflowY = x => d("overflowY", string_of_overflow(x));

  let zIndex = x => d("zIndex", string_of_int(x));

  let contentRule = x => d("content", x);

  let columnCount = x =>
    d(
      "columnCount",
      switch (x) {
      | `auto => "auto"
      | `count(v) => string_of_int(v)
      | `inherit_ => "inherit"
      | `initial => "initial"
      | `unset => "unset"
      },
    );

  /**
   * Style
   */

  let backfaceVisibility = x =>
    d("backfaceVisibility", string_of_visibility(x));

  let visibility = x => d("visibility", string_of_visibility(x));

  let boxShadow =
      (~x=zero, ~y=zero, ~blur=zero, ~spread=zero, ~inset=false, color) =>
    `shadow(
      string_of_length(x)
      ++ " "
      ++ string_of_length(y)
      ++ " "
      ++ string_of_length(blur)
      ++ " "
      ++ string_of_length(spread)
      ++ " "
      ++ string_of_color(color)
      ++ " "
      ++ (inset ? "inset" : ""),
    );

  let string_of_shadow =
    fun
    | `shadow(s) => s;
  let boxShadows = shadows =>
    d("boxShadow", shadows |> List.map(string_of_shadow) |> join(", "));

  let string_of_borderstyle =
    fun
    | `solid => "solid"
    | `dashed => "dashed"
    | `dotted => "dotted"
    | `none => "none";

  let border = (px, style, color) =>
    d(
      "border",
      string_of_length(px)
      ++ " "
      ++ string_of_borderstyle(style)
      ++ " "
      ++ string_of_color(color),
    );
  let borderWidth = x => d("borderWidth", string_of_length(x));
  let borderStyle = x => d("borderStyle", string_of_borderstyle(x));
  let borderColor = x => d("borderColor", string_of_color(x));

  let borderLeft = (px, style, color) =>
    d(
      "borderLeft",
      string_of_length(px)
      ++ " "
      ++ string_of_borderstyle(style)
      ++ " "
      ++ string_of_color(color),
    );
  let borderLeftWidth = x => d("borderLeftWidth", string_of_length(x));
  let borderLeftStyle = x => d("borderLeftStyle", string_of_borderstyle(x));
  let borderLeftColor = x => d("borderLeftColor", string_of_color(x));

  let borderRight = (px, style, color) =>
    d(
      "borderRight",
      string_of_length(px)
      ++ " "
      ++ string_of_borderstyle(style)
      ++ " "
      ++ string_of_color(color),
    );

  let borderRightWidth = x => d("borderRightWidth", string_of_length(x));
  let borderRightColor = x => d("borderRightColor", string_of_color(x));
  let borderRightStyle = x =>
    d("borderRightStyle", string_of_borderstyle(x));
  let borderTop = (px, style, color) =>
    d(
      "borderTop",
      string_of_length(px)
      ++ " "
      ++ string_of_borderstyle(style)
      ++ " "
      ++ string_of_color(color),
    );

  let borderTopWidth = x => d("borderTopWidth", string_of_length(x));
  let borderTopStyle = x => d("borderTopStyle", string_of_borderstyle(x));
  let borderTopColor = x => d("borderTopColor", string_of_color(x));

  let borderBottom = (px, style, color) =>
    d(
      "borderBottom",
      string_of_length(px)
      ++ " "
      ++ string_of_borderstyle(style)
      ++ " "
      ++ string_of_color(color),
    );
  let borderBottomWidth = x => d("borderBottomWidth", string_of_length(x));
  let borderBottomStyle = x =>
    d("borderBottomStyle", string_of_borderstyle(x));
  let borderBottomColor = x => d("borderBottomColor", string_of_color(x));

  let borderRadius = i => d("borderRadius", string_of_length(i));
  let borderTopLeftRadius = i =>
    d("borderTopLeftRadius", string_of_length(i));
  let borderTopRightRadius = i =>
    d("borderTopRightRadius", string_of_length(i));
  let borderBottomLeftRadius = i =>
    d("borderBottomLeftRadius", string_of_length(i));
  let borderBottomRightRadius = i =>
    d("borderBottomRightRadius", string_of_length(i));

  let tableLayout = x =>
    d(
      "tableLayout",
      switch (x) {
      | `auto => "auto"
      | `fixed => "fixed"
      },
    );

  let borderCollapse = x =>
    d(
      "borderCollapse",
      switch (x) {
      | `collapse => "collapse"
      | `separate => "separate"
      },
    );

  let borderSpacing = i => d("borderSpacing", string_of_length(i));

  let background = x => d("background", string_of_background(x));
  let backgrounds = bg =>
    d("background", bg |> List.map(string_of_background) |> join(", "));
  let backgroundColor = x => d("backgroundColor", string_of_color(x));
  let backgroundImage = x =>
    d(
      "backgroundImage",
      switch (x) {
      | `none => "none"
      | `url(url) => "url(" ++ url ++ ")"
      | `linearGradient(angle, stops) =>
        string_of_linearGradient(angle, stops)
      | `repeatingLinearGradient(angle, stops) =>
        string_of_repeatingLinearGradient(angle, stops)
      | `radialGradient(stops) =>
        "radial-gradient(" ++ string_of_stops(stops) ++ ")"
      | `repeatingRadialGradient(stops) =>
        "repeating-radial-gradient(" ++ string_of_stops(stops) ++ ")"
      },
    );

  let backgroundAttachment = x =>
    d(
      "backgroundAttachment",
      switch (x) {
      | `scroll => "scroll"
      | `fixed => "fixed"
      | `local => "local"
      },
    );

  let backgroundClip = x =>
    d(
      "backgroundClip",
      switch (x) {
      | `borderBox => "border-box"
      | `contentBox => "content-box"
      | `paddingBox => "padding-box"
      },
    );

  let backgroundOrigin = x =>
    d(
      "backgroundOrigin",
      switch (x) {
      | `borderBox => "border-box"
      | `contentBox => "content-box"
      | `paddingBox => "padding-box"
      },
    );

  let backgroundPosition = (x, y) =>
    d(
      "backgroundPosition",
      string_of_length(x) ++ " " ++ string_of_length(y),
    );

  let backgroundRepeat = x =>
    d(
      "backgroundRepeat",
      switch (x) {
      | `repeat => "repeat"
      | `noRepeat => "no-repeat"
      | `repeatX => "repeat-x"
      | `repeatY => "repeat-y"
      },
    );

  let backgroundSize = x =>
    d(
      "backgroundSize",
      switch (x) {
      | `size(x, y) => string_of_length(x) ++ " " ++ string_of_length(y)
      | `auto => "auto"
      | `cover => "cover"
      | `contain => "contain"
      },
    );

  let cursor = x => d("cursor", string_of_cursor(x));

  let clipPath = x =>
    d(
      "clipPath",
      switch (x) {
      | `url(url) => "url(" ++ url ++ ")"
      },
    );

  type listStyleType = [
    | `disc
    | `circle
    | `square
    | `decimal
    | `lowerAlpha
    | `upperAlpha
    | `lowerGreek
    | `lowerLatin
    | `upperLatin
    | `lowerRoman
    | `upperRoman
    | `none
  ];

  let string_of_listStyleType =
    fun
    | `disc => "disc"
    | `circle => "circle"
    | `square => "square"
    | `decimal => "decimal"
    | `lowerAlpha => "lower-alpha"
    | `upperAlpha => "upper-alpha"
    | `lowerGreek => "lower-greek"
    | `lowerLatin => "lower-latin"
    | `upperLatin => "upper-latin"
    | `lowerRoman => "lower-roman"
    | `upperRoman => "upper-roman"
    | `none => "none";

  let string_of_listStylePosition =
    fun
    | `inside => "inside"
    | `outside => "outside";

  let string_of_listStyleImage =
    fun
    | `none => "none"
    | `url(url) => "url(" ++ url ++ ")";

  let listStyle = (style, pos, img) =>
    d(
      "listStyle",
      string_of_listStyleType(style)
      ++ " "
      ++ string_of_listStylePosition(pos)
      ++ " "
      ++ string_of_listStyleImage(img),
    );

  let listStyleType = x => d("listStyleType", string_of_listStyleType(x));

  let listStylePosition = x =>
    d("listStylePosition", string_of_listStylePosition(x));

  let listStyleImage = x => d("listStyleImage", string_of_listStyleImage(x));

  let opacity = x => d("opacity", string_of_float(x));

  type outlineStyle = [
    | `none
    | `hidden
    | `dotted
    | `dashed
    | `solid
    | `double
    | `groove
    | `ridge
    | `inset
    | `outset
  ];

  let string_of_outlineStyle =
    fun
    | `none => "none"
    | `hidden => "hidden"
    | `dotted => "dotted"
    | `dashed => "dashed"
    | `solid => "solid"
    | `double => "double"
    | `groove => "grove"
    | `ridge => "ridge"
    | `inset => "inset"
    | `outset => "outset";

  let outline = (size, style, color) =>
    d(
      "outline",
      string_of_length(size)
      ++ " "
      ++ string_of_outlineStyle(style)
      ++ " "
      ++ string_of_color(color),
    );

  let outlineStyle = x => d("outlineStyle", string_of_outlineStyle(x));

  let outlineWidth = x => d("outlineWidth", string_of_length(x));

  let outlineColor = x => d("outlineColor", string_of_color(x));

  let outlineOffset = x => d("outlineOffset", string_of_length(x));

  /**
   * Text
   */

  type fontStyle = [ | `normal | `italic | `oblique];
  let fontStyleToJs =
    fun
    | `normal => "normal"
    | `italic => "italic"
    | `oblique => "oblique"
    | `inherit_ => "inherit"
    | `initial => "initial"
    | `unset => "unset";

  let color = x => d("color", string_of_color(x));

  let fontFamily = x => d("fontFamily", x);

  let fontSize = x => d("fontSize", string_of_length_cascading(x));

  let fontVariant = x =>
    d(
      "fontVariant",
      switch (x) {
      | `normal => "normal"
      | `smallCaps => "small-caps"
      },
    );

  let fontStyle = x => d("fontStyle", fontStyleToJs(x));

  let fontFace = (~fontFamily, ~src, ~fontStyle=?, ~fontWeight=?, ()) => {
    let fontStyle =
      Js.Option.map((. value) => fontStyleToJs(value), fontStyle);
    let src =
      src
      |> List.map(
           fun
           | `localUrl(value) => {j|local("$(value)")|j}
           | `url(value) => {j|url("$(value)")|j},
         )
      |> String.concat(", ");
    CssBackend.(
      makeFontFace(
        CssBackend.fontFace(~fontFamily, ~src, ~fontStyle?, ~fontWeight?),
      )
    );
  };

  let fontWeight = x => d("fontWeight", string_of_int(x));

  let lineHeight = x =>
    d(
      "lineHeight",
      switch (x) {
      | `normal => "normal"
      | `abs(x) => string_of_float(x)
      | `calc(`add, a, b) =>
        "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
      | `calc(`sub, a, b) =>
        "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
      | `ch(x) => string_of_float(x) ++ "ch"
      | `cm(x) => string_of_float(x) ++ "cm"
      | `em(x) => string_of_float(x) ++ "em"
      | `ex(x) => string_of_float(x) ++ "ex"
      | `mm(x) => string_of_float(x) ++ "mm"
      | `percent(x) => string_of_float(x) ++ "%"
      | `pt(x) => string_of_int(x) ++ "pt"
      | `px(x) => string_of_int(x) ++ "px"
      | `pxFloat(x) => string_of_float(x) ++ "px"
      | `rem(x) => string_of_float(x) ++ "rem"
      | `vh(x) => string_of_float(x) ++ "vh"
      | `vmax(x) => string_of_float(x) ++ "vmax"
      | `vmin(x) => string_of_float(x) ++ "vmin"
      | `vw(x) => string_of_float(x) ++ "vw"
      | `auto => "auto"
      | `zero => "0"
      | `inherit_ => "inherit"
      | `initial => "initial"
      | `unset => "unset"
      },
    );

  let letterSpacing = x =>
    d(
      "letterSpacing",
      switch (x) {
      | `normal => "normal"
      | `calc(`add, a, b) =>
        "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
      | `calc(`sub, a, b) =>
        "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
      | `ch(x) => string_of_float(x) ++ "ch"
      | `cm(x) => string_of_float(x) ++ "cm"
      | `em(x) => string_of_float(x) ++ "em"
      | `ex(x) => string_of_float(x) ++ "ex"
      | `mm(x) => string_of_float(x) ++ "mm"
      | `percent(x) => string_of_float(x) ++ "%"
      | `pt(x) => string_of_int(x) ++ "pt"
      | `px(x) => string_of_int(x) ++ "px"
      | `pxFloat(x) => string_of_float(x) ++ "px"
      | `rem(x) => string_of_float(x) ++ "rem"
      | `vh(x) => string_of_float(x) ++ "vh"
      | `vmax(x) => string_of_float(x) ++ "vmax"
      | `vmin(x) => string_of_float(x) ++ "vmin"
      | `vw(x) => string_of_float(x) ++ "vw"
      | `auto => "auto"
      | `zero => "0"
      },
    );

  let textAlign = x =>
    d(
      "textAlign",
      switch (x) {
      | `left => "left"
      | `right => "right"
      | `center => "center"
      | `justify => "justify"
      },
    );

  let textDecoration = x =>
    d(
      "textDecoration",
      switch (x) {
      | `none => "none"
      | `underline => "underline"
      | `overline => "overline"
      | `lineThrough => "line-through"
      },
    );

  let textDecorationColor = x =>
    d("textDecorationColor", string_of_color(x));

  let textDecorationStyle = x =>
    d(
      "textDecorationStyle",
      switch (x) {
      | `wavy => "wavy"
      | `solid => "solid"
      | `double => "double"
      | `dotted => "dotted"
      | `dashed => "dashed"
      },
    );

  let textIndent = x => d("textIndent", string_of_length(x));

  let textOverflow = x =>
    d(
      "textOverflow",
      switch (x) {
      | `clip => "clip"
      | `ellipsis => "ellipsis"
      | `string(s) => s
      },
    );

  let textShadow = (~x=zero, ~y=zero, ~blur=zero, color) =>
    d(
      "textShadow",
      string_of_length(x)
      ++ " "
      ++ string_of_length(y)
      ++ " "
      ++ string_of_length(blur)
      ++ " "
      ++ string_of_color(color),
    );

  let textTransform = x =>
    d(
      "textTransform",
      switch (x) {
      | `uppercase => "uppercase"
      | `lowercase => "lowercase"
      | `capitalize => "capitalize"
      | `none => "none"
      },
    );

  let userSelect = x =>
    d(
      "userSelect",
      switch (x) {
      | `auto => "auto"
      | `all => "all"
      | `text => "text"
      | `none => "none"
      },
    );

  let verticalAlign = x =>
    d(
      "verticalAlign",
      switch (x) {
      | `baseline => "baseline"
      | `sub => "sub"
      | `super => "super"
      | `top => "top"
      | `textTop => "text-top"
      | `middle => "middle"
      | `bottom => "bottom"
      | `textBottom => "text-bottom"
      | `calc(`add, a, b) =>
        "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
      | `calc(`sub, a, b) =>
        "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
      | `ch(x) => string_of_float(x) ++ "ch"
      | `cm(x) => string_of_float(x) ++ "cm"
      | `em(x) => string_of_float(x) ++ "em"
      | `ex(x) => string_of_float(x) ++ "ex"
      | `mm(x) => string_of_float(x) ++ "mm"
      | `percent(x) => string_of_float(x) ++ "%"
      | `pt(x) => string_of_int(x) ++ "pt"
      | `px(x) => string_of_int(x) ++ "px"
      | `pxFloat(x) => string_of_float(x) ++ "px"
      | `rem(x) => string_of_float(x) ++ "rem"
      | `vh(x) => string_of_float(x) ++ "vh"
      | `vmax(x) => string_of_float(x) ++ "vmax"
      | `vmin(x) => string_of_float(x) ++ "vmin"
      | `vw(x) => string_of_float(x) ++ "vw"
      | `auto => "auto"
      | `zero => "0"
      },
    );

  let whiteSpace = x =>
    d(
      "whiteSpace",
      switch (x) {
      | `normal => "normal"
      | `nowrap => "nowrap"
      | `pre => "pre"
      | `preLine => "pre-line"
      | `preWrap => "pre-wrap"
      },
    );

  let wordBreak = x =>
    d(
      "wordBreak",
      switch (x) {
      | `breakAll => "break-all"
      | `keepAll => "keep-all"
      | `normal => "normal"
      },
    );

  let wordSpacing = x =>
    d(
      "wordSpacing",
      switch (x) {
      | `normal => "normal"
      | `calc(`add, a, b) =>
        "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
      | `calc(`sub, a, b) =>
        "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
      | `ch(x) => string_of_float(x) ++ "ch"
      | `cm(x) => string_of_float(x) ++ "cm"
      | `em(x) => string_of_float(x) ++ "em"
      | `ex(x) => string_of_float(x) ++ "ex"
      | `mm(x) => string_of_float(x) ++ "mm"
      | `percent(x) => string_of_float(x) ++ "%"
      | `pt(x) => string_of_int(x) ++ "pt"
      | `px(x) => string_of_int(x) ++ "px"
      | `pxFloat(x) => string_of_float(x) ++ "px"
      | `rem(x) => string_of_float(x) ++ "rem"
      | `vh(x) => string_of_float(x) ++ "vh"
      | `vmax(x) => string_of_float(x) ++ "vmax"
      | `vmin(x) => string_of_float(x) ++ "vmin"
      | `vw(x) => string_of_float(x) ++ "vw"
      | `auto => "auto"
      | `zero => "0"
      },
    );

  let wordWrap = x =>
    d(
      "wordWrap",
      switch (x) {
      | `normal => "normal"
      | `breakWord => "break-word"
      },
    );

  let string_of_pointerEvents =
    fun
    | `auto => "auto"
    | `none => "none";

  let pointerEvents = x => d("pointerEvents", string_of_pointerEvents(x));

  /**
 * Transform
 */

  type transform = [
    | `translate(length, length)
    | `translate3d(length, length, length)
    | `translateX(length)
    | `translateY(length)
    | `translateZ(length)
    | `scale(float, float)
    | `scale3d(float, float, float)
    | `scaleX(float)
    | `scaleY(float)
    | `scaleZ(float)
    | `rotate(angle)
    | `rotate3d(float, float, float, angle)
    | `rotateX(angle)
    | `rotateY(angle)
    | `rotateZ(angle)
    | `skew(angle, angle)
    | `skewX(angle)
    | `skewY(angle)
    | `perspective(int)
  ];

  let string_of_transform =
    fun
    | `translate(x, y) =>
      "translate("
      ++ string_of_length(x)
      ++ ", "
      ++ string_of_length(y)
      ++ ")"
    | `translate3d(x, y, z) => string_of_translate3d(x, y, z)
    | `translateX(x) => "translateX(" ++ string_of_length(x) ++ ")"
    | `translateY(y) => "translateY(" ++ string_of_length(y) ++ ")"
    | `translateZ(z) => "translateZ(" ++ string_of_length(z) ++ ")"
    | `scale(x, y) => string_of_scale(x, y)
    | `scale3d(x, y, z) =>
      "scale3d("
      ++ string_of_float(x)
      ++ ", "
      ++ string_of_float(y)
      ++ ", "
      ++ string_of_float(z)
      ++ ")"
    | `scaleX(x) => "scaleX(" ++ string_of_float(x) ++ ")"
    | `scaleY(y) => "scaleY(" ++ string_of_float(y) ++ ")"
    | `scaleZ(z) => "scaleZ(" ++ string_of_float(z) ++ ")"
    | `rotate(a) => "rotate(" ++ string_of_angle(a) ++ ")"
    | `rotate3d(x, y, z, a) =>
      "rotate3d("
      ++ string_of_float(x)
      ++ ", "
      ++ string_of_float(y)
      ++ ", "
      ++ string_of_float(z)
      ++ ", "
      ++ string_of_angle(a)
      ++ ")"
    | `rotateX(a) => "rotateX(" ++ string_of_angle(a) ++ ")"
    | `rotateY(a) => "rotateY(" ++ string_of_angle(a) ++ ")"
    | `rotateZ(a) => "rotateZ(" ++ string_of_angle(a) ++ ")"
    | `skew(x, y) =>
      "skew(" ++ string_of_angle(x) ++ ", " ++ string_of_angle(y) ++ ")"
    | `skewX(a) => "skewX(" ++ string_of_angle(a) ++ ")"
    | `skewY(a) => "skewY(" ++ string_of_angle(a) ++ ")"
    | `perspective(x) => "perspective(" ++ string_of_int(x) ++ ")";

  let transform = x => d("transform", string_of_transform(x));

  let transforms = xs =>
    d("transform", xs |> List.map(string_of_transform) |> join(" "));

  let transformOrigin = (x, y) =>
    d("transformOrigin", string_of_length(x) ++ " " ++ string_of_length(y));

  let transformOrigin3d = (x, y, z) =>
    d(
      "transformOrigin",
      string_of_length(x)
      ++ " "
      ++ string_of_length(y)
      ++ " "
      ++ string_of_length(z)
      ++ " ",
    );

  let transformStyle = x =>
    d(
      "transformStyle",
      switch (x) {
      | `preserve3d => "preserve-3d"
      | `flat => "flat"
      },
    );

  let perspective = x =>
    d(
      "perspective",
      switch (x) {
      | `none => "none"
      | `calc(`add, a, b) =>
        "calc(" ++ string_of_length(a) ++ " + " ++ string_of_length(b) ++ ")"
      | `calc(`sub, a, b) =>
        "calc(" ++ string_of_length(a) ++ " - " ++ string_of_length(b) ++ ")"
      | `ch(x) => string_of_float(x) ++ "ch"
      | `cm(x) => string_of_float(x) ++ "cm"
      | `em(x) => string_of_float(x) ++ "em"
      | `ex(x) => string_of_float(x) ++ "ex"
      | `mm(x) => string_of_float(x) ++ "mm"
      | `percent(x) => string_of_float(x) ++ "%"
      | `pt(x) => string_of_int(x) ++ "pt"
      | `px(x) => string_of_int(x) ++ "px"
      | `pxFloat(x) => string_of_float(x) ++ "px"
      | `rem(x) => string_of_float(x) ++ "rem"
      | `vh(x) => string_of_float(x) ++ "vh"
      | `vmax(x) => string_of_float(x) ++ "vmax"
      | `vmin(x) => string_of_float(x) ++ "vmin"
      | `vw(x) => string_of_float(x) ++ "vw"
      | `zero => "0"
      },
    );

  /**
   * Transition
   */

  type timingFunction = [
    | `linear
    | `ease
    | `easeIn
    | `easeOut
    | `easeInOut
    | `stepStart
    | `stepEnd
    | `steps(int, [ | `start | `end_])
    | `cubicBezier(float, float, float, float)
  ];

  let string_of_timingFunction =
    fun
    | `linear => "linear"
    | `ease => "ease"
    | `easeIn => "ease-out"
    | `easeOut => "ease-out"
    | `easeInOut => "ease-in-out"
    | `stepStart => "step-start"
    | `stepEnd => "step-end"
    | `steps(i, `start) => "steps(" ++ string_of_int(i) ++ ", start)"
    | `steps(i, `end_) => "steps(" ++ string_of_int(i) ++ ", end)"
    | `cubicBezier(a, b, c, d) =>
      "cubic-bezier("
      ++ string_of_float(a)
      ++ ", "
      ++ string_of_float(b)
      ++ ", "
      ++ string_of_float(c)
      ++ ", "
      ++ string_of_float(d)
      ++ ")";

  let transition = (~duration=0, ~delay=0, ~timingFunction=`ease, property) =>
    `transition(
      string_of_time(duration)
      ++ " "
      ++ string_of_timingFunction(timingFunction)
      ++ " "
      ++ string_of_time(delay)
      ++ " "
      ++ property,
    );

  let transitions = xs =>
    d(
      "transition",
      xs
      |> List.map(
           fun
           | `transition(s) => s,
         )
      |> join(", "),
    );

  let transitionDelay = i => d("transitionDelay", string_of_time(i));

  let transitionDuration = i => d("transitionDuration", string_of_time(i));

  let transitionTimingFunction = x =>
    d("transitionTimingFunction", string_of_timingFunction(x));

  let transitionProperty = x => d("transitionProperty", x);

  let perspectiveOrigin = (x, y) =>
    d(
      "perspectiveOrigin",
      string_of_length(x) ++ " " ++ string_of_length(y),
    );

  /**
   * Animation
   */

  type animationDirection = [
    | `normal
    | `reverse
    | `alternate
    | `alternateReverse
  ];

  let string_of_animationDirection =
    fun
    | `normal => "normal"
    | `reverse => "reverse"
    | `alternate => "alternate"
    | `alternateReverse => "alternate-reverse";

  type animationFillMode = [ | `none | `forwards | `backwards | `both];

  let string_of_animationFillMode =
    fun
    | `none => "none"
    | `forwards => "forwards"
    | `backwards => "backwards"
    | `both => "both";

  type animationIterationCount = [ | `infinite | `count(int)];

  let string_of_animationIterationCount =
    fun
    | `infinite => "infinite"
    | `count(x) => string_of_int(x);

  type animationPlayState = [ | `paused | `running];

  let string_of_animationPlayState =
    fun
    | `paused => "paused"
    | `running => "running";

  let animation =
      (
        ~duration=0,
        ~delay=0,
        ~direction=`normal,
        ~timingFunction=`ease,
        ~fillMode=`none,
        ~playState=`running,
        ~iterationCount=`count(1),
        name,
      ) =>
    `animation(
      name
      ++ " "
      ++ string_of_time(duration)
      ++ " "
      ++ string_of_timingFunction(timingFunction)
      ++ " "
      ++ string_of_time(delay)
      ++ " "
      ++ string_of_animationIterationCount(iterationCount)
      ++ " "
      ++ string_of_animationDirection(direction)
      ++ " "
      ++ string_of_animationFillMode(fillMode)
      ++ " "
      ++ string_of_animationPlayState(playState),
    );

  let string_of_animation =
    fun
    | `animation(s) => s;
  let animations = xs =>
    d("animation", xs |> List.map(string_of_animation) |> join(", "));

  let animationDelay = x => d("animationDelay", string_of_time(x));
  let animationDirection = x =>
    d("animationDirection", string_of_animationDirection(x));
  let animationDuration = x => d("animationDuration", string_of_time(x));
  let animationFillMode = x =>
    d("animationFillMode", string_of_animationFillMode(x));
  let animationIterationCount = x =>
    d("animationIterationCount", string_of_animationIterationCount(x));
  let animationName = x => d("animationName", x);
  let animationPlayState = x =>
    d("animationPlayState", string_of_animationPlayState(x));
  let animationTimingFunction = x =>
    d("animationTimingFunction", string_of_timingFunction(x));

  /**
   * Selectors
   */

  let selector = (selector, rules) => `selector((selector, rules));

  /* MEDIA */

  let active = selector(":active");
  let after = selector("::after");
  let before = selector("::before");
  let checked = selector(":checked");
  let children = selector(" > *");
  let directSibling = selector(" + ");
  let disabled = selector(":disabled");
  let firstChild = selector(":first-child");
  let firstOfType = selector(":first-of-type");
  let focus = selector(":focus");
  let hover = selector(":hover");
  let lastChild = selector(":last-child");
  let lastOfType = selector(":last-of-type");
  let link = selector(":link");
  let readOnly = selector(":read-only");
  let required = selector(":required");
  let visited = selector(":visited");
  let enabled = selector(":enabled");
  let noContent = selector(":empty");
  let default = selector(":default");
  let anyLink = selector(":any-link");
  let onlyChild = selector(":only-child");
  let onlyOfType = selector(":only-of-type");
  let optional = selector(":optional");
  let invalid = selector(":invalid");
  let outOfRange = selector(":out-of-range");
  let siblings = selector(" ~ ");
  let target = selector(":target");
  let firstLine = selector("::first-line");
  let firstLetter = selector("::first-letter");
  let selection = selector("::selection");
  let placeholder = selector("::placeholder");

  let media = (query, rules) => `selector(("@media " ++ query, rules));

  /**
   * SVG
   */
  module SVG = {
    let fill = color => d("fill", string_of_color(color));
    let fillOpacity = opacity => d("fillOpacity", string_of_float(opacity));
    let fillRule = x =>
      d(
        "fillRule",
        switch (x) {
        | `evenodd => "evenodd"
        | `nonzero => "nonzero"
        },
      );
    let stroke = color => d("stroke", string_of_color(color));
    let strokeWidth = length => d("strokeWidth", string_of_length(length));
    let strokeOpacity = opacity =>
      d("strokeOpacity", string_of_float(opacity));
    let strokeMiterlimit = x => d("strokeMiterlimit", string_of_float(x));
    let strokeLinecap = x =>
      d(
        "strokeLinecap",
        switch (x) {
        | `butt => "butt"
        | `round => "round"
        | `square => "square"
        },
      );

    let strokeLinejoin = x =>
      d(
        "strokeLinejoin",
        switch (x) {
        | `miter => "miter"
        | `round => "round"
        | `bevel => "bevel"
        },
      );
    let stopColor = c => d("stopColor", string_of_color(c));
    let stopOpacity = o => d("stopOpacity", string_of_float(o));
  };
};
