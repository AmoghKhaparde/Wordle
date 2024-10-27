extends Node2D

# this button quits the game if the user chooses
func _on_nuh_button_up() -> void:
	get_tree().quit()

# this button starts the game if the user wants to
func _on_play_now_button_down() -> void:
	get_tree().change_scene_to_file("res://main.tscn")

# this tells the instructions to the user and centers the text
func _ready() -> void:
	$backgroundMusicMenu.playing = true
	$textBox.text = "[center]You got 6 tries to guess a random five letter word. Green letters are in the
right spot. Yellow letters are in the word, but in the wrong spot. Black letters are wrong and not in the word. Good Luck...
[/center]"

func _on_background_music_menu_finished() -> void: # play the background music of the menu
	$backgroundMusicMenu.playing = true
