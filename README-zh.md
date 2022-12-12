# Org-Mode + Spectacle.js = (简单 + 漂亮 + 强大) 的幻灯片

基于 html5 的幻灯片，有 [reveal.js](https://revealjs.com/)、[impress.js](https://impress.js.org/) 等。但在我看来，`reveal.js` 过于朴素且语法复杂，`impress.js` 看起来很好实际上功能缺失太多。也曾使用过其他幻灯片方案，比如 `beamer` 和 `org-present` 等，要么太复杂，要么太简陋，都不能令人满意。

横竖左右比较下来，最实用、也最好看的是 [Spectacle.js](https://formidable.com/open-source/spectacle/)。使用 [React](https://reactjs.org/) 写幻灯片？很疯狂，但确实非常好用。

但是，由于基于 `React`，它的上手难度有些高。即便熟练掌握 `React` 的人，为了幻灯片写大量的 `React.js` 代码也是一件很折磨人的事。

鱼与熊掌不可兼得吗？错了，借助 Org Mode 强大的 Export 引擎，便可以轻轻松松利用 `Spectacle.js` 创造出漂亮的幻灯片了。这就是 `ox-spectacle` 产生的原因。

基本特点 (设计目标):
1. 简单。不要有太多的语法，尽量减少心智负担，一切保持简单
2. 全面。包含 `Spectacle.js` 中的的所有功能，可以自由创作和扩展
2. 自包含。允许将脚本、图片内嵌到一个单独的 html 中，从而便于分发或离线使用

## 基本使用

首先，通过 Melpa 或其他方式安装本 `ox-spectacle` 包。

然后，加载之:
```elisp
  (require 'ox-spectacle)
```

随后，创建 org 文件。比如，下面是包含两个页面的的幻灯片:
```org
#+TITLE: 示例幻灯片

* 第一个页面

   这是第一个页面。

   #+ATTR_HTML: :width 200 :style {{ margin: "2em" }}
   [[https://formidable.com/open-source/spectacle/svg/formidable-logo-white.svg]]

   #+ATTR_HTML: :margin {2} :width {900} :showLineNumbers {true}
   #+begin_src html
     <Slide>
       <Text>我是第一个页面</Text>
       <Image width="200"
              style={{ margin: "2em" }}
              src="https://.../formidable-logo-white.svg" />
     </Slide>
   #+end_src

* 第二个页面

   #+ATTR_HTML: :type Heading :fontFamily Kaiti
   列表和动画演示

   - 从前有座山，山中有座庙
   - 庙里住个老和尚，在那讲故事
     1. <A> 从前有座山，山中有座庙
     2. <A> <span style={{color: "red"}}>庙里住个老和尚，在那讲故事……</span>
        + <A> 从前有座山……
   - 故事讲完。谁赞成，谁反对？
```

通过 `C-c C-e s o` 导出为 html 并在浏览器中打开。完工。

## 示例幻灯片

相应文件在 examples 文件夹中，可自行下载查看。

| 介绍  | Org 文件 | 效果展示，可点击查看 |
|:-----:|:------:|:-------:|
| 【基本使用】中的幻灯片，仅包含两个页面 | [first_zh.org](https://raw.githubusercontent.com/lorniu/ox-spectacle/master/examples/first_zh.org) | [first_zh.html](https://lorniu.github.io/spectacle/first_zh.html) |
| [Spectacle 官方示例](https://github.com/FormidableLabs/spectacle/tree/main/examples/js) 中的幻灯片，使用 Org/Ox-Spectacle 进行重写 | [official.org](https://raw.githubusercontent.com/lorniu/ox-spectacle/master/examples/official.org) | [official.html](https://lorniu.github.io/spectacle/official.html)|
| 一个会持续更新的示例 (**推荐**)  | [scratch.org](https://raw.githubusercontent.com/lorniu/ox-spectacle/master/examples/scratch.org) | [scratch.html](https://lorniu.github.io/spectacle/scratch.html)|

## 预备知识

在正式开始之前，你需要懂一点 html、[React](https://reactjs.org/) 和 [Spectacle.js](https://formidable.com/open-source/spectacle/docs/)。

一个典型的 Spectacle.js 的幻灯片是这样的结构:
```jsx
<Deck theme=.. template=.. backgroundImage=.. style=..>
  <Slide>
     <Box|FlexBox|Grid...>
        <Heading|Text|Image|Link...>内容或参数</Heading|Text|Image|Link>
     </Box|FlexBox|Grid...>
  </Slide>
  <Slide></Slide>
  <Slide></Slide>
</Deck>
```

几句话总结一下:
- 最顶层的节点为 `Deck`，表示整个幻灯片。`Deck` 下面有若干 `Slide` 或 `SlideLayout.Xxx`，每个表示一个幻灯片页面
- 在页面中使用各种组件构建内容。Spectacle 提供了 `Heading/Text/Link/Image/Table/CodePane/Appear` 等多种组件，可以非常方便表达并渲染标题、文本、列表、表格、图片、动画等。Spectacle 也提供了 `Box/FlexBox/Grid` 等组件用作容器，对应 html 中的 `display=block|flex|grid`
- 每个组件都可以传入若干属性 (props)，它们可能是 css 样式，也可能是组件自定义的其他属性。组件具备的属性详情，需要通过文档了解。传入属性需遵循 `ReactJS` 语法规则，否则会报错。比如可以写成 `backgroundColor="red"` 或 `fontSize={22}` 或 `style={{ color: "red", fontSize: 22 }}` 这样的形式。`{}` 包括起来的是一段合法的 JavaScript 代码或数据
- 比如，可以向 `Deck` 传入 `theme`、`template`、`transition`、`backgroundImage` 等属性，用来设定幻灯片的主题颜色、页面模板、转场动画、背景图片等。向 `Slide` 或其他组件传入类似的属性，将只会影响当前组件
- 背后的一切都是 `html/css/js`，可以按照规则任意创建并使用自己的组件

## 语法规则

> 现在开始简单扼要介绍 ox-spectacle 的语法规则。更多的细节，请查看 examples 文件或阅读源码。

### 基本规则

顶层的 headline (level 1) 默认渲染为 `<Slide>` 组件，可使用 `PROPERTY DRAWER` 进行设定:
- 通过 `:layout:` 可将渲染组件替换为 SlideLayout。比如 `:layout: Center` 会将当前 headline 渲染为 `<SlideLayout.Center>`
- 当一个 headline 当设定了 `:layout: top` 那么其子 headline 才会被渲染为 `Slide`！
- 通过 `:props:` 为组件传入属性。比如 `:props: backgroundImage="url(...)" backgroundOpacity={0.5}` 将为当前页面设定半透明的背景图片

非顶层的 headline 默认会被渲染成 `<Box>` 组件，同样使用 `PROPERTY DRAWER` 进行设定:
- 通过 `:type:` 设定渲染的目标组件。比如 `:type: FlexBox` 会将当前 headline 下面的内容包装在一个 `<FlexBox>` 中
- 通过 `:props:` 为组件传入属性
- 如果 headline 的标题是 `<Grid props...>` 这样的组件声明格式，那么将会使用 headline 上的声明作为当前的容器组件

页面中的普通文本段落 (paragraph) 默认会被渲染为 `<Text>xxx</Text>` 组件格式:
- 可以在上面添加 `#+ATTR_HTML:` 进行设定:
  + 语法为 `#+ATTR_HTML: :aaa 111 :bbb {222}`，其值需要遵循 ReactJS 语法规则
  + 通过 `:type` 设定目标组件，比如 `:type Heading` 会将当前内容渲染为 `<Heading>xxx</Heading>` 组件格式
  + 通过这种方式可向组件传递更多参数，比如 `#+ATTR_HTML: :type p :color "red" :style {{ marginTop: 2 }}`
- 如果当前文本段落符合 `<Grid props...>` 这样的组件语法，将不会进行任意转换。因此，可在 org 中自由地写原生 Spectacle 代码
- 为了方便原生代码的编写，可以通过 `M-x ox-spectacle-minor-mode` 激活一些额外的代码高亮和补全 (`capf`)

综上所述，对于同样的页面，存在多种不同的实现方式。

```org
* Slide1, 原生写法

  <FlexBox margin={2}>
    <Heading color="blue">Hello</Heading>
    <Appear>
      <Text color="yellow">World</Text>
    </Appear>
  </FlexBox>

--------------------------------
* Slide2, 纯粹 Org 写法
** 这是一个弹性盒子
  :PROPERTIES:
  :type: FlexBox
  :props: margin={2}
  :END:

  #+ATTR_HTML: :type Heading :color blue
  Hello

*** 这是一个分步显示的内容
   :PROPERTIES:
   :type: Appear
   :END:

    #+ATTR_HTML: :color yellow
    World

--------------------------------
* Slide3, 混搭模式
** <FlexBox margin={2}>

   #+ATTR_HTML: :type Heading :color blue
   Hello

*** <Appear>

    #+ATTR_HTML: :color yellow
    World

--------------------------------
* Slide4，居中显示并设定背景
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

不同场景各种写法各有优劣。不要有心智包袱，想怎么写怎么写。了解其规则，合理使用即可。

如果想向页面中添加图片、表格、列表、代码块等，可以使用原生的 Spectacle 写法，但是更推荐使用 Org 语法，这样可以大幅度提高代码的简洁性。同样使用 `#+ATTR_HTML:` 向背后的组件传递属性。

```org
图片 (<Image>):

  [[file:xxx.png]]

  #+ATTR_HTML: :width 200px
  [[file:xxx.png]]

表格 (<Table>):

  | name | price |
  |------+-------|
  | aaa  |   222 |
  | bbb  |   333 |

列表 (<OrderedList/UnorderedList>):

  - aaa
    1. 111
    2. 222
  - bbb
  - ccc

代码 (<CodePane>):

  #+ATTR_HTML: :marginBottom {2}
  #+begin_src js
    const aaa = (e) => {
        console.log(e);
    }
  #+end_src
```

### 容器组件

最主要的容器组件为 `<Box>`、`<FlexBox>`、`<Grid>`，它们分别对应 html 中的 block/flexbox/grid，尤其 flexbox 和 grid 是 html 标准中较新的东西，它们非常灵活非常强大。掌握了它们，就能对页面中各部分的布局做到随心所欲了:
- https://developer.mozilla.org/zh-CN/docs/Web/CSS/CSS_Flexible_Box_Layout/Basic_Concepts_of_Flexbox
- https://developer.mozilla.org/zh-CN/docs/Web/CSS/CSS_Grid_Layout/Basic_Concepts_of_Grid_Layout

容器组件的属性跟 html 中对应的 css 属性基本一致。这里不赘述。

### 简化的分步显示

Spectacle 中使用 `<Appear>` 和 `<Stepper>` 组件来控制分步显示:
```html
  <Appear priority={1}> 第一个 </Appear>
  <Appear priority={2}> 第二个 </Appear>

  <!-- 更复杂的控制，需要用到 Stepper -->
  <Stepper tagName="p" values={['foo', 'bar']}>
    {(value, step, isActive) =>
      isActive
        ? 'The first stepper is not active. Step:' + step + ' Value:' + value :
        'The first stepper is active. Step:' + step + ' Value:' + value
    }
  </Stepper>
```

Appear 使用相对频繁，因此这里提供了若干简化语法。

可以通过 `#+ATTR_HTML` 向 Image/Code/Table 组件传递 `:type A`，从而为其修饰 Appear:
```org
  使用 :type A 表示图片用 Appear 包围:

    #+ATTR_HTML: :type A
    [[file:ssss.png]]

  等同于:

    <Appear>[[file:ssss.png]]</Appear>

  A 后面可以跟更多参数，比如下面 priority={3} 将传递给 Appear。注意语法细节:

    #+ATTR_HTML: :type A priority={3} :margin {2} :border "1px solid red"
    [[file:ssss.png]]

  可以使用 :type NUM 直接表示 priority 为 NUM 的 Appear:

    #+ATTR_HTML: :type 2 activeStyle={color: "red"}
    [[file:ssss.png]]

  代码块和表格使用同样的机制。不赘述:

    #+ATTR_HTML: :type A
    #+begin_src js
      const aaa = 3
    #+end_src
```

列表条目的分步显示，简化语法类似，只不过是通过 `<A props>` 的方式传递分步参数:
```org
- 普通的正常显示的条目
- <A> 如果在前面存在 <A> 那么本项将参与到分步显示中
- <2> 如果在前面存在 <数字> 那么本项将会参与到分步显示，并且其显示的优先级 (priority) 为括号中的数字
- <1> 这个的优先级要比上面的高，所以会比上面的先显示
- <A activeStyle={{ color: "red" }}> 可以为 Appear 传入参数
- <color="red" fontSize="1px"> 如果尖括号内不以 A 或 NUM 开始，那么这些属性将传递给 ListItem
- <A color="red" inactiveStyle={{color: "red"}}> 对于这样的形式，priority/inactiveStyle 等属性将传递给 Appear，其他将传递给 ListItem
```

借助上述方式，可以节省大量 Appear，让代码更简洁。

### 特殊的 `<config>` 节点

可以在 Org 文件中添加一个标题为 `<config>` 的特殊 headline。其中的内容将不会被 export engine 导出，而仅作配置使用。

其基本规则如下:
- `<config>` 下如果存在命名格式为 `** <template> xxx` 的二级 headline，其下内容将会生成名为 `xxx` 的模板
- `<config>` 下存在的任意 `js/javascript` 代码片段将会被抓取并插入到页面刚开始的 script 中
- `<config>` 下存在的任意 `css` 代码片段将会被抓取并插入到页面的 head/style 中
- `<config>` 下存在的任意 `html` 代码片段将会被抓取并插入到页面的 head 下面

> 另外，其他 headline 下任意通过 `#+ATTR_HTML: :type config` 标注的代码段也不会被导出，它们同样循序上述的后 3 条规则。

示例一: 定义模板，命名为 tp1 和 tp2
```org
* <config>
** <template> tp1

  <Text position="absolute" top={0} right={0}>欢迎使用 Spectacle</Text>

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

示例二: 定义主题，命名为 theme1、theme2 和 theme3
```org
* <config>
** 可以在 <template> 除外的任意位置定义

  #+begin_src js
    const theme1 = {
        fonts: {
            header: '"Open Sans Condensed", Helvetica, Arial, sans-serif',
            text: '"Open Sans Condensed", Helvetica, Arial, sans-serif'
        }
    };

    const theme2 = defaultTheme;
  #+end_src

** 可以分散到任意代码块中

  #+begin_src js
    const theme3 = { ...defaultTheme, ... }
  #+end_src
```

示例三: 定义动画效果，命名为 ts1 和 ts2
```org
* <config>

  #+begin_src js
    const ts1 = fadeTransition;  // slideTransition, defaultTransition
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

示例四: 引入额外的 JS 支持
```org
* <config>

  #+begin_src html
    <script src="jquery.js"></script>
    <style rel="stylesheet" href="bootstrap.css">
  #+end_src
```

示例五: 添加一个自定义组件:
```org
* <config>

  注意，这里的代码需要符合 react-htm 的语法规则。

      https://github.com/developit/htm

  这里创建的组件或引入的第三方组件，需要通过在 org 中注册才能正常使用:

      #+EXTERN_COMPONENTS: MyDeck

  组件的创建代码:

  #+begin_src js
    const MyDeck = (props) => {
        html`<div class="my-deck-wrapper">$<{Deck} ...${props}></${Deck}></div>`;
    }
  #+end_src
```

示例六: 添加若干全局 style 样式
```org
* <config>

  #+begin_src css
    .my-deck-wrapper { zoom: 0.7 }
    image:hover { opacity: 0.6 }
    @keyframes bigger { from { width: 100px } to { width: 150px }}
  #+end_src
```

## 配置选项

全局配置关键词, all here:
- `#+THEME/TEMPLATE/TRANSITION` 设定全局主题、模板、过场动画。这些选项将被传递给 Deck 节点
- `#+DECK_OPTS` 对幻灯片进行全局配置。这些属性将会传递给 Deck 节点
- `#+SLIDE_OPTS` 替换顶层 headline 默认的 `<Slide>` 目标组件
- `#+TEXT_OPTS` 替换 paragraph 默认的 `<Text>` 目标组件
- `#+EXTRA_SCRIPTS` 其他的页面中需要使用的 script 文件，最好用这种方式引入
- `#+EXTERN_COMPONENTS` 自定义或第三方组件，需要在这里声明之后才能正常使用
- `#+EXPORT_LEVEL` 设定导出策略。默认为 0 正常导出。为 1 则将所有 scripts 内嵌到导出的 html 中。为 2 则将所有 images 内嵌到导出的 html 中。如果为 3 则将 scripts 和 images 都内嵌到导出的 html 中，即实现 all-in-one/self-contained 式的导出

使用示例:
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

#+EXTRA_SCRIPTS: https://unpkg.com/recharts/umd/Recharts.js
#+EXTERN_COMPONENTS: MyDeck MyLink Recharts

#+EXPORT_LEVEL: 3
```

可以为某个 Slide 页面或组件单独配置 theme/template/transition 等。示例:
```org
* 使用了单独各种设置的一个Slide
 :PROPERTIES:
 :props: theme={theme2} template={tp2} transition={ts2}
 :END:

  #+ATTR_HTML: :theme={theme3}
  #+begin_src sh
    ls
  #+end_src
```

## 其他

临时想到这么多，更多内容酌情补充。

欢迎提供想法和建议。
