package inf112.utils;

import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.math.Rectangle;
import inf112.gfx.Canvas;
import inf112.gfx.Palette;

import java.util.HashMap;

/**
 * A simple UI helper class with buttons, sliders, checkboxes, and textboxes.
 */
public class UI {

    /**
     * Draws a button and checks if it's clicked.
     *
     * @param canvas Where to draw the button.
     * @param x      X position.
     * @param y      Y position.
     * @param width  Button width.
     * @param height Button height.
     * @param text   Text on the button.
     * @return True if clicked, false otherwise.
     */
    public static boolean button(Canvas canvas, int x, int y, int width, int height, String text) {
        return button(canvas, x, y, width, height, text, null);
    }

    public static boolean button(Canvas canvas, int x, int y, int width, int height, String text, Color color) {
        Vec2 mousePos = canvas.getMouseWorldPosition();

        Rectangle buttonBounds = new Rectangle(x, y, width, height);
        boolean isHovered = buttonBounds.contains(mousePos.x, mousePos.y);

        Color buttonColor;
        if (color == null) {
            buttonColor = isHovered ? Palette.BUTTON_HOVER : Palette.BUTTON;
        } else {
            buttonColor = isHovered ? Palette.BUTTON : color;
        }
		canvas.drawRect(x, y, width, height, buttonColor);

        // Draw text
        int textLen = !text.isEmpty() ? text.length() : 1;
        int textSize = height / (textLen * 10);
        Vec2 textPos = new Vec2(x + 10.f, y - 10 + (float) height / 2);
        canvas.drawText(text, textPos, Palette.BUTTON_TEXT, textSize);

        return isHovered && Mouse.clicked();
    }

    private static final HashMap<String, Boolean> isDraggingMap = new HashMap<>();
    private static final HashMap<String, Integer> handleX = new HashMap<>();

    /**
     * Creates a slider that can be dragged.
     *
     * @param canvas Where to draw.
     * @param x      X position.
     * @param y      Y position.
     * @param min    Minimum value.
     * @param max    Maximum value.
     * @param size   Scale factor (w = 200 * size, h = 10 * size).
     * @param name   Unique ID for the slider.
     * @return Current slider value.
     */
    public static int slider(Canvas canvas, int x, int y, int min, int max, int size, String name, int initialValue) {
        if (initialValue > max || initialValue < min) {
            throw new Error(
                    "Slider value must be in the interval min: " + min + " max: " + max);
        }

        int sliderWidth = 200 * size;
        int sliderHeight = 10 * size;
        int handleSize = 35;

        int initialHandleX = x + (int) (((float) (initialValue - min) / (max - min)) * (sliderWidth - handleSize));

        handleX.putIfAbsent(name, initialHandleX);
        isDraggingMap.putIfAbsent(name, false);

        boolean isDragging = isDraggingMap.get(name);
        int thisHandleX = handleX.get(name);

        Vec2 mousePos = canvas.getMouseWorldPosition();
        Rectangle buttonBounds = new Rectangle(thisHandleX, y, handleSize, handleSize);

        if (Mouse.down()) {
            if (buttonBounds.contains(mousePos.x, mousePos.y)) {
                isDragging = true;
            }
        } else {
            isDragging = false;
        }

        isDraggingMap.put(name, isDragging);

        if (isDragging) {
            thisHandleX = (int) mousePos.x - (handleSize / 2);
        }

        thisHandleX = Math.max(x, Math.min(thisHandleX, x + sliderWidth - handleSize));
        handleX.put(name, thisHandleX);

        canvas.drawRect(x, y, sliderWidth, sliderHeight, Palette.SLIDER);
        button(canvas, thisHandleX, y, handleSize, handleSize, "");

        float normalized = (float) (thisHandleX - x) / (sliderWidth - handleSize);
        int sliderValue = Math.round(normalized * (max - min)) + min;

        canvas.drawText(String.valueOf(sliderValue), new Vec2(x, y - 100.f), Palette.FOREGROUND, 5);

        return sliderValue;
    }

    private static final HashMap<String, Boolean> checkBoxMap = new HashMap<>();
    private static long lastClickTime = 0;

    /**
     * Creates a simple checkbox that can be toggled.
     *
     * @param canvas Where to draw.
     * @param x      X position.
     * @param y      Y position.
     * @param width  Checkbox width.
     * @param height Checkbox height.
     * @param id     Unique ID.
     * @return True if checked, false otherwise.
     */
    public static boolean checkbox(Canvas canvas, int x, int y, int width, int height, String id) {
        checkBoxMap.putIfAbsent(id, false);

        Rectangle buttonBounds = new Rectangle(x, y, width, height);
        Vec2 mousePos = canvas.getMouseWorldPosition();

        boolean isHovered = buttonBounds.contains(mousePos.x, mousePos.y);
        Color buttonColor = checkBoxMap.get(id) ? Palette.CHECKBOX : Palette.CHECKBOX_HOVER;

        canvas.drawRect(x, y, width, height, buttonColor);
        long currentTime = System.currentTimeMillis();
        if (isHovered && Mouse.clicked() && (currentTime - lastClickTime > 200)) {
            checkBoxMap.put(id, !checkBoxMap.get(id));
            lastClickTime = currentTime;
        }

        return checkBoxMap.get(id);
    }

    private static final HashMap<String, String> textBoxMap = new HashMap<>();
    private static final HashMap<String, Boolean> textBoxActiveMap = new HashMap<>();

    /**
     * Creates a simple text box for user input.
     *
     * @param canvas Where to draw.
     * @param x      X position.
     * @param y      Y position.
     * @param width  Text box width.
     * @param height Text box height.
     * @param id     Unique ID.
     * @return User input text.
     */
    public static String textBox(Canvas canvas, int x, int y, int width, int height, String id) {
        textBoxMap.putIfAbsent(id, "");
        textBoxActiveMap.putIfAbsent(id, false);

        String input = textBoxMap.get(id);
        boolean isActive = textBoxActiveMap.get(id);

        Rectangle textBoxBounds = new Rectangle(x, y, width, height);
        Vec2 mousePos = canvas.getMouseWorldPosition();
        if (Mouse.clicked()) {
            textBoxActiveMap.put(id, textBoxBounds.contains(mousePos.x, mousePos.y));
        }

        Color backgroundColor = isActive ? Palette.TEXTBOX_ACTIVE : Palette.TEXTBOX_UNACTIVE;
        canvas.drawRect(x, y, width, height, backgroundColor);
        canvas.drawText(input, new Vec2(x + 10.f, y + (float) height / 2), Palette.FOREGROUND, 3);

        if (isActive) {
            String keyboardInput = Keyboard.input();
            if (keyboardInput != null) {
                input = keyboardInput.equals("BACKSPACE") && !input.isEmpty() ? input.substring(0, input.length() - 1)
                        : input + keyboardInput;
                textBoxMap.put(id, input);
            }
        }

        return input;
    }
}
