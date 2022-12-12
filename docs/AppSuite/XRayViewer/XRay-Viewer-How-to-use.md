# How to use

Once the program runs, you will have to select an image in a folder containing multiple images or an hdf5 file. The program will automatically display the selected item. You will see a new window as below:

![-](../../images/XV/xv_image.png)

You can see the index of the image displayed in the bottom left corner of the window. The result tab is empty at first.

* Click on "Set Graph Slice" to set the graph. This function will allow you to draw a line on the image that will be displayed in the result tab. The length of the line doesn't matter, the slice will follow the line but will display the entire length of the image (you can use a graph box without witdh if you want only a selected length).

* Click on "Set Graph Box" to set the graph. This function will allow you to draw a line on the image, then select a width, and will display in the result tab the integrated slice (sum of the rows). The length of the line matters, the slice will follow the line and will only display the selected part.

![-](../../images/XV/xv_graph.png)

* You can save the graphs displayed by checking the "Save Graph Profile" box. It will be saved as a single csv file named `summary.csv` in `xv_results`. To save a graph, you need to go to the next/previous one with the arrows, or with the "Play" button

* Use the arrows to go from one image/graph to the other. Use the "Play" button to display each image/graph one by one like a short video.
