Impress.scm 
============================================

杨至轩

1300012785@pku.edu.cn

Introduction
------------------
我没有选择做积分系统或者通用算术包（同裘老师说明过，得到了老师的同意），而是实现了一个用于编辑和生成幻灯片的系统。用户按照本系统提供的语言编写好幻灯片之后可以自动生成出html格式的幻灯片，然后用Google Chrome/Firefox等浏览器播放即可。

系统提供的用于编写幻灯片的DSL在racket之上实现，样例代码如下：

```scheme
(load "impress.scm")

(define (mylayout content)
  (fixed-height-n-rows-box
    (list 0.2 0.6 0.2)
    (list (empty-content) content (empty-content))
    '()))

(impress
  (font-size "24px")
  (slide-cover "impress.scm!"
               "elegant slides written in scheme" 
               "yang zhixuan")
  (slide
    (mylayout 
      (ordered-list 
        (text "write down what you want to present in impress.scm")
        (text "generate htmls automatically")
        (text "play the slides in Google Chrome!"))))

  (slide
    (mylayout
      (text "you can (easily) extend impress.scm with your own layout, javascript library, ...")))

  (slide-cover
    "thank you!")
  
  (slide-flow-style 
    "circle"  2000)
  
  (output-filename "templates/demo.html"))
```

在工程的文件夹中，执行`racket -f demo.scm`或在REPL中`(load "demo.scm")`即会在template/子目录下生成demo.html(包里附带了已经生成好的文件)，然后用chrome等浏览器打开demo.html即可播放幻灯片(按方向键或空格键翻页)。


Background
-------------------

由于Linux环境下缺乏有效的幻灯片制作软件，我一直以来都用html5 + css3 + javascript的方式制作幻灯片，这样的幻灯片([impress.js](http://bartaz.github.io/impress.js/))可以达到很好的视觉效果，可扩展性非常强(可以使用数不胜数的javascript库)，唯一的缺点是编写比较麻烦，需要手写html和css也使得这种方式的使用者比较少，虽然有人做过尝试编写一些所见即所得的工具，但这样的工具往往难于扩展，丧失了html幻灯片的一大优点。

本项目impress.scm为幻灯片和html之间提供了一层抽象。想要制作幻灯片的用户无需考虑具体的html标签和css规则等底层细节，而在impress.scm提供的幻灯片布局、幻灯片元素的抽象之上工作。impress.scm被设计为可以容易的进行扩展，熟悉scheme的用户可以很容易的为它加入新的功能。

Structure
------------------
项目包含如下几个文件：

- impress.scm： 对各种content对象的实现

- properties.scm： 对各种property对象的实现

- layouts.scm： 对各种layout对象的实现

- misc.scm：一些工具函数，如用来生成html文本的函数

- test.scm：编写过程中用的一些测试

impress.scm以消息传递的风格组织而成，主要有两类对象：content对象和property对象。

- content对象描述幻灯片中某个具体元素，如一段文本，一个图片，一张幻灯片。

- property对象描述content对象的一个属性，如font-size, background-color等。

- 还有一类特殊的content对象：layout对象，用于描述幻灯片内容的布局，如“两栏布局”，“多行布局”等。

content对象的构造函数（如上案例的make-slide函数）往往接收任意多个参数，每个参数要么也是一个content对象，要么是property对象。被接收的property对象用于描述本content对象的行为，如`(make-slide (font-size "12px"))`将使得这个slide的字体大小是12px。构造函数返回的content对象能够处理`'type`, `'to-html`等消息。

property对象的构造函数也接受任意多个参数，返回的对象能够处理`'type`, `'key`, `'val`等消息。由于需要定义大量的property对象，于是使用了一些宏来消除重复。

layout对象也是content对象的一种，默认提供了n-rows-layout, n-columns-layout等基本的layout，用户可以用这些layout组合出复杂的layout。


Future Works
-----------------

1. 改进对幻灯片布局的描述方法，目前对布局的描述方法有一定限制，不适用于需要在任意座标出现的元素。

2. 改进对Javascript库的支持，目前的接口不便于使用Javascript库。

3. 对制作幻灯片的易用性的进一步提高。

4. 更友好的错误提示和更健壮的系统。
