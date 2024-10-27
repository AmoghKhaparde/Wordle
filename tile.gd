extends Node2D

var TEXT = "" # holds text
var TYPED = false # keeps track of if the next slot is available
var wrongPosition = false
var notLetter = false
var correctLetter = false


# this just keeps the letters of the game board in one spot
func _process(delta):
	$RichTextLabel.text = "[center]%s[/center]" % TEXT
	$RichTextLabel.visible_characters = 1;
	
