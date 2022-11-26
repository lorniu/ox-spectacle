[![License: GPL-3.0](http://img.shields.io/:license-gpl3-blue.svg)](https://opensource.org/licenses/GPL-3.0)
[![MELPA](https://melpa.org/packages/ox-spectacle-badge.svg)](https://melpa.org/#/ox-spectacle)

[🤟 **点击查看《中文版文档》**](README-zh.md)

I've been looking for solutions to create slideshows other than `PowerPoint` or `Keynote`. I tried `reveal.js`, `impress.js`, `beamer`, `org-present` and others, but but none of them satisfies me.

Then I found [Spectacle.js](https://formidable.com/open-source/spectacle/), a library creating slideshows based on `React.js`.
Create a slideshow using React.js? It's hard to Imagine, but it does work perfect.

However, since it is based on React, it is a bit hard to get started.
Even for those who are skilled in React, writing a lot of React code for the slides can be a torturous task.

Then I create this `ox-spectacle.el`, a spectacle.js backend for org export engine. With this, maybe you'll own the **Easiest way to create the Most Beautiful and Most Powerful Slides**. At least, it's another solution for you to create slideshows :)

> `Spectacle.js` is the best tool to create slides with html5, and this `ox-spectacle.el` is the best way to create slides with Spectacle.js.
>
> `Org Mode` + `Spectacle.js` = The final Solution

## Get Started

First, install this package with Melpa or other way.

Then, load `ox-spectacle` when necessary:
```elisp
  (require 'ox-spectacle)
```

Then create an org file and put your ideas there. For example, a presentation with two slides:
```org
#+TITLE: Demo Slides

* The First Slide

   This is the first slide.

   #+ATTR_HTML: :width 200 :style {{ margin: "2em" }}
   [[https://formidable.com/open-source/spectacle/svg/formidable-logo-white.svg]]

   #+ATTR_HTML: :margin {2} :width {900} :showLineNumbers {true}
   #+begin_src html
     <Slide>
       <Text>This is the first slide.</Text>
       <Image width="200"
              style={{ margin: "2em" }}
              src="https://.../formidable-logo-white.svg" />
     </Slide>
   #+end_src

* The Second Slide

   #+ATTR_HTML: :type Heading :fontFamily serif
   List And Appear

   - Once, there was a river
   - Full of water
     1. <A> I Lost my shoes there
     2. <A> <span style={{color: "red"}}>But I washed my hair</span>
        + <A> and feet ..
   - This is the story
```

Export and open it with `C-c C-e s o`, and the **amazing** presentation is in front of you. Enjoy it.

## Examples

You can find the files under `examples` fold.

| Description  | Org File | Click to View |
|:-----:|:------:|:-------:|
| Slides in 'Get Started' section | [first_en.org](https://raw.githubusercontent.com/lorniu/ox-spectacle/master/examples/first_en.org) | [first_en.html](https://lorniu.github.io/spectacle/first_en.html) |
| Slides in [Spectacle Official Example](https://github.com/FormidableLabs/spectacle/tree/main/examples/js), rewritten with Org/Ox-Spectacle | [official.org](https://raw.githubusercontent.com/lorniu/ox-spectacle/master/examples/official.org) | [official.html](https://lorniu.github.io/spectacle/official.html)|
| A continually updated slideshow (**recommanded**)  | [scratch.org](https://raw.githubusercontent.com/lorniu/ox-spectacle/master/examples/scratch.org) | [scratch.html](https://lorniu.github.io/spectacle/scratch.html)|

## Prep Knowledge

You should learn a bit html, [React](https://reactjs.org/) and [Spectacle.js](https://formidable.com/open-source/spectacle/docs/) before `ox-spectacle`.

A presentation written with `spectacle.js` is like this:
```jsx
<Deck theme=.. template=.. backgroundImage=.. style=..>
  <Slide>
     <Box|FlexBox|Grid...>
        <Heading|Text|Image|Link...>props and content</Heading|Text|Image|Link>
     </Box|FlexBox|Grid...>
  </Slide>
  <Slide></Slide>
  <Slide></Slide>
</Deck>
```

Brief summary:
- The topmost node is `Deck`, which represents the presentation. It contains a number of `Slide` or `SlideLayout.Xxx`, each representing a slide page
- Spectacle provides many components to build the slide page, like `Heading/Text/Link/Image/Table/CodePane/Appear` for contents and `Box/FlexBox/Grid` as containers
- Each component can be passed in a number of `props`, which may be css styles or other properties customized by the component. The props must follow the `ReactJS` syntax rules. It's written like `backgroundColor="red"` or `fontSize={22}` or `style={{ color: "red", fontSize: 22 }}`. `{}` encloses a valid JavaScript code or data
- For example, you can pass props `theme/template/transition/backgroundImage` to `Deck` to set slideshow theme color and so on. Passing props to `Slide` or other components will only affect that component
- Everything behind is `html/css/js`, so you can create and use your own components as you wish

## Syntax Rules of ox-spectacle

> Now is a brief overview of the syntax rules of ox-spectacle. For more
> details, please view the example files or read the source code.

### Basic Rules

The top-level headline is rendered by default as `<Slide>`, it can be set using `PROPERTY DRAWER`:
- Use `:layout` to replace the rendered component with `<SlideLayout>`. For example `:layout: Center` will render the current headline as `<SlideLayout.Center>`
- Use `:props:` to pass in props to the component. For example `:props: backgroundImage="url(...)" backgroundOpacity={0.5}` will set a translucent background for the current slide

Other headlines is rendered by default as `<Box>`, it can be set using `PROPERTY DRAWER` too:
- Use `:type:` to replace the rendered component. For example `:type: FlexBox` will render the contents as `<FlexBox>contents</FlexBox>`
- Use `:props:` to pass in props to the component as well
- If the title of the headline is a component declaration like `<Grid props...>`, the declarated component will be the rendered component instead

The plain paragraph of text is rendered as `<Text>` by default:
- It can be set with `#+ATTR_HTML:` above. For example `#+ATTR_HTML: :color "red" :margin {5}`. The value should follow the `ReactJS` syntax rules too.
  + Use `:type:` to change the target component. For example, `:type Heading` will render the paragraph to `<Heading>text</Heading>`
  + Pass in props to the component the same way. For example, `#+ATTR_HTML: :type p :class "abc" :style {{ marginTop: 2 }}` will render text to `<p class="abc" style=...>text</p>` style
- If the paragraph is component notation like `<Component props...>`, then will be no any conversion. Therefore, native Spectacle code can be freely written
- You can activate some extra code highlighting and completion (`capf`) by `M-x ox-spectacle-minor-mode` to get a better code experience in writting native Spectacle

All in all, for the same slide, there are many ways of writing.

```org
* Slide1, Spectacle Syntax

  <FlexBox margin={2}>
    <Heading color="blue">Hello</Heading>
    <Appear>
      <Text color="yellow">World</Text>
    </Appear>
  </FlexBox>

--------------------------------
* Slide2, Normal Org Syntax
** This is a flexbox
  :PROPERTIES:
  :type: FlexBox
  :props: margin={2}
  :END:

  #+ATTR_HTML: :type Heading :color blue
  Hello

*** This is a stepper
   :PROPERTIES:
   :type: Appear
   :END:

    #+ATTR_HTML: :color yellow
    World

--------------------------------
* Slide3, Mix Both Styles
** <FlexBox margin={2}>

   #+ATTR_HTML: :type Heading :color blue
   Hello

*** <Appear>

    #+ATTR_HTML: :color yellow
    World

--------------------------------
* Slide4，Center with SlideLayout and set background
  :PROPERTIES:
  :layout: Center
  :props: backgroundImage="url(xxx.png)" backgroundOpacity={0.5}
  :END:

** <FlexBox margin={2}>

   #+ATTR_HTML: :type Heading :color blue
   Hello

*** <Appear>

    #+ATTR_HTML: :color yellow
    World
```

There are pros and cons to different writing styles for
different scenes. Write whatever you want for free.
Just understand the rules and use them wisely.

You can use the native Spectacle style to add images/tables/lists/code/etc, but recommended to use the Org syntax, which can greatly improve the
simplicity of the code. Pass props to the component behind with `#+ATTR_HTML:` as well.

```org
image (<Image>):

  [[file:xxx.png]]

  #+ATTR_HTML: :width 200px
  [[file:xxx.png]]

table (<Table>):

  | name | price |
  |------+-------|
  | aaa  |   222 |
  | bbb  |   333 |

list (<OrderedList/UnorderedList>):

  - aaa
    1. 111
    2. 222
  - bbb
  - ccc

code (<CodePane>):

  #+ATTR_HTML: :marginBottom {2}
  #+begin_src js
    const aaa = (e) => {
        console.log(e);
    }
  #+end_src
```

### Container Components

The main container components are `<Box>`, `<FlexBox>`, `<Grid>`.
They correspond to `display=block/flex/grid` in html. Especially flexbox and
grid are newer in html standard, they are very flexible and
powerful, and can help you layout pages easily.

- https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flexible_Box_Layout/Basic_Concepts_of_Flexbox
- https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout/Basic_Concepts_of_Grid_Layout

The props of these components are basically the same as the corresponding css properties. You'd better learn them by yourself.

### Stepper on list/image/code/table

Spectacle makes the step-by-step display with component `<Appear>` and `<Stepper>`:
```html
  <Appear priority={1}> First Appear </Appear>

  <Appear priority={2}> Second Appear </Appear>

  <!-- Stepper is for more complex control -->
  <Stepper tagName="p" values={['foo', 'bar']}>
    {(value, step, isActive) =>
      isActive
        ? 'The first stepper is not active. Step:' + step + ' Value:' + value :
        'The first stepper is active. Step:' + step + ' Value:' + value
    }
  </Stepper>
```

Appear is used relatively frequently, so some simplified syntax is provided.

You can pass `:type A/NUM` with `#+ATTR_HTML:` to image/code/table to make it Appear by step:
```org
  Use `:type A` to make sure the Image is around with Appear:

    #+ATTR_HTML: :type A
    [[file:ssss.png]]

  That is:

    <Appear>[[file:ssss.png]]</Appear>

  Can pass props to Appear, for example, pass `priority={3}` to Appear:

    #+ATTR_HTML: :type A priority={3} :margin {2} :border "1px solid red"
    [[file:ssss.png]]

  Use `:type NUM` to make a Appear with priority as NUM:

    #+ATTR_HTML: :type 2 activeStyle={color: "red"}
    [[file:ssss.png]]

  The same `:type` syntax can also be used on code block and table:

    #+ATTR_HTML: :type A
    #+begin_src js
      const aaa = 3
    #+end_src
```

The step-by-step display of list item use the same syntax. You can pass the Appear props use `<A props>` format:
```org
- item that normal display
- <A> if <A> ahead the item, then display this by step
- <2> if <NUM> ahead the item, display by step with priority of NUM
- <1> this has a higher priority than the item above
- <A activeStyle={{ color: "red" }}> pass in props to Appear this way
- <color="red" fontSize="1px"> without the prefix A or NUM, the props will pass to ListItem
- <A color="red" inactiveStyle={{color: "red"}}> priority/activeStyle/alwaysVisible.. will pass to Appear, and others to ListItem
```
Proper use of these syntaxes can greatly simplify the code.

### Special `<config>` headline

The top-most headline titled with `* <config>` is only used for configuration. It will not be exported.

The rules are simple. Under the `* <config>`:
- Headline titled as `** <template> xxx` will generate and assign to `xxx` as a template
- Contents in all `js/javascript` code blocks will be inserted into **script** section of the final html
- Contents in all `css` code blocks will be inserted into **style** section of the final html
- Contents in all `html` code blocks will be inserted into **head** section of the final html

> In addition, any code block out of `<config>` marked with `#+ATTR_HTML: :type config` will not be exported and follow the last 3 rules above too.

Demo 1: define templates `tp1` and `tp2`
```org

* <config>
** <template> tp1

  <Text position="absolute" top={0} right={0}>Spectacle</Text>

** <template> tp2

  <Box margin="-15px" backgroundOpacity={0.5} >
    <Text color="white" fontSize={10}>WELCOME</Text>
  </Box>

  <FlexBox justifyContent='space-between' position='absolute' bottom={0} width={1}>
    <Box padding='0 1em'>
    </Box>
    <FlexBox padding='0.5em' backgroundColor='red'>
      <Progress size={5} />
      <Text fontSize={6}>${slideNumber + '/' + numberOfSlides}</{Text>
    </FlexBox>
  </FlexBox>
```

Demo 2: define themes `theme1`, `theme2` and `theme3`
```org
* <config>
** code blocks can be anywhere other than under <template>

  #+begin_src js
    const theme1 = {
        fonts: {
            header: '"Open Sans Condensed", Helvetica, Arial, sans-serif',
            text: '"Open Sans Condensed", Helvetica, Arial, sans-serif'
        }
    };

    const theme2 = defaultTheme;
  #+end_src

** as many as you wish

  #+begin_src js
    const theme3 = { ...defaultTheme, ... }
  #+end_src
```

Demo 3: define transitions `ts1` and `ts2`
```org
* <config>

  #+begin_src js
    const ts1 = fadeTransition; // slideTransition, defaultTransition
    const ts2 = {
        from: {
            transform: 'scale(0.5) rotate(45deg)',
            opacity: 0
        },
        enter: {
            transform: 'scale(1) rotate(0)',
            opacity: 1
        },
        leave: {
            transform: 'scale(0.2) rotate(315deg)',
            opacity: 0
        }
    };
  #+end_src
```

Demo 4: import external scripts or styles?
```org
* <config>

  #+begin_src html
    <script src="https://unpkg.com/browse/chart.js@2.7.1/dist/Chart.bundle.min.js"></script>
  #+end_src
```

Demo 5: define your own component
```org
* <config>

  Notice, here you should follow the syntax of react-htm:

      https://github.com/developit/htm

  The components you create or import should be regist before use:

      #+EXTERN_COMPONENTS: MyDeck

  Component definition:

  #+begin_src js
    const MyDeck = (props) => {
        html`<div class="my-deck-wrapper">$<{Deck} ...${props}></${Deck}></div>`;
    }
  #+end_src
```

Demo 6: add some global styles
```org
* <config>

  #+begin_src css
    .my-deck-wrapper { zoom: 0.7 }
    image:hover { opacity: 0.6 }
    @keyframes bigger { from { width: 100px } to { width: 150px }}
  #+end_src
```

## Configuration Options

All global configuration options:
- `#+THEME/TEMPLATE/TRANSITION` options passed to Deck node to set global theme and so on
- `#+DECK_OPTS` options passed to Deck node as extra props
- `#+SLIDE_OPTS` change the default `<Slide>` used for headline render
- `#+TEXT_OPTS` change the default `<Text>` used for plain text paragraph render
- `#+EXTERN_COMPONENTS` declare user/third-party components before use them
- `#+EXPORT_LEVEL` Set export policy. Defaults 0 for normal export. If 1 then embed all scripts into the exported html. If 2 embed all images into the exported html. If >=3 embedded all scripts and images into the exported html, that is, all-in-one file

Usage example:
```org
#+TITLE: DEMO

#+THEME: theme1
#+TEMPLATE: tp1
#+TRANSITION: ts1

#+DECK_OPTS: MyDeck expertMode={true} overviewMode={false}
#+DECK_OPTS: useAnimations={true} autoPlay={false} autoPlayInterval={2000}
#+DECK_OPTS: onSlideClick={(e,s) => console.log('current slide: ' + s)}
#+DECK_OPTS: backdropStyle={{border: "0px solid skyblue"}}

#+SLIDE_OPTS: backgroundColor="white"
#+TEXT_OPTS: Text color="grey" fontSize={30}
# #+TEXT_OPTS: p

#+EXTERN_COMPONENTS: MyDeck MyLink

#+EXPORT_LEVEL: 3
```

You can set separate config `theme/template/transition` for some slides or components:
```org
* Slide with their own theme/template/transition
 :PROPERTIES:
 :props: theme={theme2} template={tp2} transition={ts2}
 :END:

  #+ATTR_HTML: :theme={theme3}
  #+begin_src sh
    ls
  #+end_src
```

## Miscellaneous

Thanks [formidable](https://formidable.com/open-source/spectacle/) for the excellent work, spectacle.js is great. Thanks emacs and org-mode. Thanks you all.
