# My StumpWM config

This is the repo with my chaotic customisations of StumpWM. Although it's chaotic and long, there are two parts that I plan to enhance and move to separate contrib files. Until this happens, this README will contain explanation and documentation for these two contributions: `definition` command and `binwarp` mouse-replacing mode.

## definition

Yet Another Dictionary Lookup Contrib for StumpWM. This function is my optimization of reading books in English. I sometimes need to look up the definitions and examples of usage of some English words. The idea behind this function is that the Browser-Search-engine-Dictionary-website loop is mostly inevitable and the old ways (like CSV dictionaries) are still cool but aren't that flexible and easily updatable.  

That's why scraping web-pages, regexp-ing over them, and then prettifying the output was a way to go. Although usage of regexps to sanitize or format HTML is highly discouraged, it's not that sophisticated or critical of a case to use something more practical (Although using [Plump](https://github.com/Shinmera/plump/) might be a worthy alternative).  

The source for the definitions is the Princeton university's [WordNet system](https://wordnet.princeton.edu/) and, in particular, the [webwn](http://wordnetweb.princeton.edu/perl/webwn) tool. Being a safe balance between academic and practical worlds, it's perfect for looking up the words from the range of literature that I happen to read from time to time: from the Victorian pathetic novels to the modern research to the documentation or standard of some hackish tool.  

Given these parts, it's a simple one-screen command that does everything that I needed, with the biggest amount of code spent on wrestling with HTML (Okay, using Plump starts sounding like a good idea while I'm writing these lines). You can safely use it in your own config. **The requirements are [Dexador](https://github.com/fukamachi/dexador) and [CL-PPCRE](https://github.com/edicl/cl-ppcre)**.

## binwarp-mode

The touchpad of my laptop worked well... until it didn't. So, I needed to solve this problem. And, as a good tradition goes, if you can't solve the problem at once, split it into two smaller problems, if you can't solve the smaller problem, split it in two... You've got the idea. 

**Binwarp** stands for **binary warping**, because it is inspired by both binary divide-and-conquer algorithms and StumpWM built-in `ratwarp` and `ratrelwarp` commands. And the "bin" part is the key part here because it's about how binwarp-mode works -- you split your screen in two and put your mouse pointer in the center of this binwarp-area. Pretty simple, yet pretty effective.

I feel obliged to say that, although I came up with this idea all by myself, there is a much more mature solution with the same approach out there: [keynav](https://github.com/jordansissel/keynav). Why bothering rewriting it all anew then? Well, I prefer customizable lisp-based systems, as you may have guessed looking at the list of the software I customize in this repo. Having a small neat function close to your heart, with as less outer dependencies as possible and written all by yourself is comfortable, fast (because CL is hella faster than most other languages!) and fun because hacking things from scratch in your config is the best unproductive activity a programmer might have, especially if it makes them more productive in their working environment! Yak shaving, you know.
