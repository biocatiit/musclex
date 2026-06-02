stylesheet = """

/* Add a border to the top-level window */
QMainWindow{
    border: 0px solid black; /* Set border color and thickness */
    border-radius: 10px; /* Optional: Add rounded corners */
}

/* Add a border to QDialog windows */
QDialog {
    border: 2px solid black; /* Set border color and thickness */
    border-radius: 10px; /* Optional: Add rounded corners */
}

/* Add a border to QDialog windows */
QMessageBox {
    border: 2px solid black; /* Set border color and thickness */
    border-radius: 10px; /* Optional: Add rounded corners */
}

/* General style for all disabled widgets */
*:disabled {
    color: gray; /* Change text color to gray */
}

/* Specific style for disabled QPushButton */
QPushButton:disabled {
    color: darkgray; /* Change text color to dark gray */
    background-color: lightgray; /* Change background color to light gray */
    border: 1px solid gray; /* Change border color to gray */
}

/* Specific style for disabled QLineEdit */
QLineEdit:disabled {
    color: darkgray; /* Change text color to dark gray */
    background-color: lightgray; /* Change background color to light gray */
    border: 1px solid gray; /* Change border color to gray */
}

/* Specific style for disabled QComboBox */
QComboBox:disabled {
    color: darkgray; /* Change text color to dark gray */
    background-color: lightgray; /* Change background color to light gray */
    border: 1px solid gray; /* Change border color to gray */
}

"""