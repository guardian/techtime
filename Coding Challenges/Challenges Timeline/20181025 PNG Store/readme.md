## PNG Store

### Challenge

This week's challenge is to implement a command line tool, called **pngstore**, that allows one to "hide" files into a .png file. It must work like this

Given a png file, for instance, **image.png** and a, say, Microsoft Word **report.doc** 

```
pngstore embed image.png report.doc image2.png
```

creates the png file **image2.png** which when open is the same picture as the original **image.png** but has embedded inside it the binary data of **report.doc** (One would expect that the file size of **image2.png** is bigger than of **image.png**)

For the example above I choose a Word document, but it should work with **any** file. Even png files, or any other type of picture. 

Then

```
pngstore extract image2.png
```

Creates the file **report.doc**. You must notice how the file name was also embedded to be able to recreate **report.doc** with the correct filename.

### PNG Specs

The [Wikipedia page](https://en.wikipedia.org/wiki/Portable_Network_Graphics) should be very useful, it has a rather good description of the PNG specs.

### Note

I implemented this tool many years ago, and used it to store various data on free-to-use online image sharing services. I was using them as storage for binary data, giving them the data embedded in pictures and only having to store the URL of the images. In fact, I then turned that into a slow and cumbersome but perfectly working FUSE file system. At the time I thought that was hilarious :)