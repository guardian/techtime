Weekly challenge (20180712)

### Introduction

Today's challenge is around Image Manipulation on the Front End. The `template` folder contains the files you will be working with. If you open the `index.html` file in a web browser you will see 4 pictures. Each picture is a black and white picture. The black area is always in one single piece, but not necessary the white area. All pictures have dimensions **400px*400px**.

### The challenge

Your job is to write a piece of JavaScript to add a red circle, 1px thick, to each picture. Your circle **must** be contained within the white area of the picture.

An example is the file `image2-circle.png`, which contains a possible circle that you code will draw. The file `index-circle.html` refers to it to show you what it should look like. 

### Rules

- You cannot modify the following files: `images/*`, `index.html`, `style.css`

- You can/should replace `index.js` with the entry point to your code, but without renaming that file (as it is referrenced from `index.html`). 

- You can add any additional files for your solution.

- Your solution must work for both Firefox and Chrome (latest versions). 

### How to submit your solution ?

1. Check out the "techtime" git repository on your local. 
1. In the `20180712-Weekly/solutions` folder create a sub-folder with your name, put your entire solution in it. Put your own augmented copy of the `template` folder in it.
1. Make a PR.

### How is Pascal going to check the solutions ?

Step 1: I will open your solution in a web-browser and see whether circles eventually appear at the right places. 

Step 2: I will replace the existing four pictures with four additional secret test pictures and rerun your solution.

### Who gets the points

The first person to make a correct submission gets **5 points**. There is no deadline for this.

The second person to make a correct submission gets **2.5 points**.

If there is more than one correct solution, there is additional challenge. This additional challenge has a deadline: Exactly 48 hours after the second PR containing a correct solution.  

The winner of this secondary challenge will be the person whose solution generated the biggest circles (that still must be valid circles, eg: totally included in white area of the pictures) on the secret test pictures. The metric I will use is the sum of the diameters of all your circles. Because of this, you might want your code to be deterministic, to avoid random fluctuations during my review. This additional challenge is worth **3 points**. 

Example: if Alice makes a first correct submission, she gets **5 points**. Then later on, Beatrice makes one. Beatrice gets **2.5 points**. Then, if within 48 hours of Beatrice's submission both Chloe and Daniella make correct submissions, and Chloe's solution displays biggers circles than everybody else, Chloe gets **3 points**.

I will publish the test pictures, when all is done. 

 