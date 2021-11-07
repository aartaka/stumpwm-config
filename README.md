# My StumpWM config

This is the repo with my chaotic customisations of StumpWM. Although it's chaotic and long, there are two parts that I plan to enhance and move to separate contrib files. Until this happens, this README will contain explanation and documentation for these two contributions: `definition` command and `binwarp` mouse-replacing mode.

## binwarp-mode

Binwarp-mode is now a part of [stumpwm-contrib](https://github.com/stumpwm/stumpwm-contrib)! Use it from there :) What follows is a small historical reference:

The touchpad of my laptop worked well... until it didn't. So, I needed to solve this problem. And, as a good tradition goes, if you can't solve the problem at once, split it into two smaller problems, if you can't solve the smaller problem, split it in two... You've got the idea. 

**Binwarp** stands for **binary warping**, because it is inspired by both binary divide-and-conquer algorithms and StumpWM built-in `ratwarp` and `ratrelwarp` commands. And the "bin" part is the key part here because it's about how binwarp-mode works -- you split your screen in two and put your mouse pointer in the center of this binwarp-area. Pretty simple, yet pretty effective.

I feel obliged to say that, although I came up with this idea all by myself, there is a much more mature solution with the same approach out there: [keynav](https://github.com/jordansissel/keynav). Why bothering rewriting it all anew then? Well, I prefer customizable lisp-based systems, as you may have guessed looking at the list of the software I customize in this repo. Having a small neat function close to your heart, with as less outer dependencies as possible and written all by yourself is comfortable, fast (because CL is hella faster than most other languages!) and fun because hacking things from scratch in your config is the best unproductive activity a programmer might have, especially if it makes them more productive in their working environment! Yak shaving, you know.
