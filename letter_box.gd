extends Node2D

var TEXT = ""
# all we do here is hold characters to show the user

func _process(delta):
	$RichTextLabel.text = "[center]%s[/center]" % TEXT
	$RichTextLabel.visible_characters = 1;
