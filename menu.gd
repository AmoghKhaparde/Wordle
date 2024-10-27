extends Node2D

func _ready() -> void:
	$backgroundMusicMenu.playing = true # turn on the background music of the menu

# this button goes to the instructions before the game starts
func _on_instruct_button_down() -> void:
	get_tree().change_scene_to_file("res://instructions.tscn")

# this button starts the game immediately
func _on_lets_play_button_up() -> void:
	get_tree().change_scene_to_file("res://main.tscn")

# this one gives the user the option to quit the game (which some real life games do have)
func _on_nah_button_down() -> void:
	get_tree().quit()

func _on_background_music_menu_finished() -> void: # play the background music of the menu
	$backgroundMusicMenu.playing = true
