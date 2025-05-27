package inf112.gfx.ui;

import inf112.gfx.Anchor;
import inf112.gfx.DrawableType;
import inf112.gfx.Interactable;
import inf112.utils.Mouse;
import inf112.utils.Vec2;

public class Slider implements Interactable {

	private final Vec2 pos;
	private Vec2 size;
	private final Vec2 offset;
	private final Vec2 range;
	private final Vec2 buttonPos;
	private final Vec2 buttonSize;
	private boolean selected;
	private final SliderCallback onDrag;

	public Slider(SliderCallback onDrag, Vec2 range, Vec2 pos) { this(onDrag,range,pos,1);
	}

	public Slider(SliderCallback onDrag,Vec2 range, Vec2 pos, int scale) {
		this(onDrag,range,pos,scale, 0);
	}

	public Slider(SliderCallback onDrag,Vec2 range, Vec2 pos, int scale, int start) {
		this(onDrag,range,pos,scale,start, Anchor.START);
	}

	public Slider(SliderCallback onDrag,Vec2 range, Vec2 pos, int scale, int startVal, Anchor anchor){
		this.onDrag = onDrag;
		this.range = range;
		this.pos = pos;
		selected = false;
		size = new Vec2(400,10);
		size = size.scl(scale);
		buttonSize = new Vec2(25,25).scl(scale);

		if (anchor == Anchor.CENTER) {
			offset = new Vec2(size.x / 2, 0);
		} else if (anchor == Anchor.END) {
			offset = new Vec2(size.x, 0);
		} else {
			offset = new Vec2(0, 0);
		}

		if(!(startVal >= range.x && startVal <= range.y)){
			throw new IllegalArgumentException("startVal must be between range.x and range.y");
		}


		float t = (startVal - range.x) / (range.y - range.x);
		float buttonX  = pos.x - offset.x + size.x * t;
		float buttonY = pos.y - size.y / 2;
		buttonPos = new Vec2(buttonX,buttonY);
	}

	@Override
	public boolean hover(boolean isHovered) {
		return false;
	}

	@Override
	public void click(boolean isClicked) {
		if(isClicked){
			selected = true;
		}else if (!Mouse.down()){
			selected = false;
		}
		if(selected){
			onDrag.callback(this);
			buttonPos.x = Mouse.pos().x - buttonSize.x / 2;
			if(buttonPos.x > pos.x + size.x){
				buttonPos.x = pos.x + size.x;
			}
			if(buttonPos.x < pos.x){
				buttonPos.x = pos.x;
			}
		}
	}

	@Override
	public DrawableType type() {
		return DrawableType.SLIDER;
	}

	@Override
	public float width() {
		return buttonSize.x;
	}

	@Override
	public float height() {
		return buttonSize.y;
	}

	@Override
	public Vec2 pos() {
		return buttonPos;
	}

	public Vec2 getSliderPos() {
		return pos;
	}

	public Vec2 getSliderSize() {
		return size;
	}

	public int getValue() {

		float barStartX = pos.x - offset.x;
		float barWidth = size.x;

		float t = (buttonPos.x - barStartX) / barWidth;

		return (int)(range.x + t * (range.y - range.x));
	}
}
