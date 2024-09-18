package labyrinth;

import labyrinth.gui.LabyrinthGUI;

public class Main {
	// Labyrinth
	public static char[][] testLabyrinth = { //
			{ '*', '*', '*', '*' }, //
			{ '*', ' ', ' ', '*' }, //
			{ '*', ' ', '*', '*' }, //
			{ '*', 's', '*', '*' }, //
			{ '*', '*', '*', '*' }, };

	public static void main(String[] args) {
		//LabyrinthGUI.run(() -> new Labyrinth(LabyrinthHelper.loadGrid(testLabyrinth)));
		LabyrinthGUI.run(() -> new Labyrinth(LabyrinthHelper.makeRandomGrid(20, 30)));
	}
}
