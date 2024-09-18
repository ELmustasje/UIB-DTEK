package no.uib.inf101.gridview;

import java.awt.*;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.*;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImageObserver;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.RenderableImage;
import java.text.AttributedCharacterIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * A dummy Graphics2D that records all shapes and colors that are filled
 * instead of actually drawing anything.
 */
public class RecordGraphics2D extends java.awt.Graphics2D {

  private Color color = null;
  private final List<Shape> recordedFillShapes = new ArrayList<>();
  private final List<Color> recordedFillColors = new ArrayList<>();

  public List<Shape> getRecordedFillShapes() {
    return this.recordedFillShapes;
  }

  public List<Color> getRecordedFillColors() {
    return this.recordedFillColors;
  }

  @Override
  public void fill(Shape s) {
    this.recordedFillShapes.add(s);
    this.recordedFillColors.add(this.color);
  }

  @Override
  public void draw(Shape s) {
    // dummy ignores
  }

  @Override
  public boolean drawImage(Image img, AffineTransform xform, ImageObserver obs) {
    return false;
  }

  @Override
  public void drawImage(BufferedImage img, BufferedImageOp op, int x, int y) {
    // dummy ignores
  }

  @Override
  public void drawRenderedImage(RenderedImage img, AffineTransform xform) {
    // dummy ignores
  }

  @Override
  public void drawRenderableImage(RenderableImage img, AffineTransform xform) {
    // dummy ignores
  }

  @Override
  public void drawString(String str, int x, int y) {
    // dummy ignores
  }

  @Override
  public void drawString(String str, float x, float y) {
    // dummy ignores
  }

  @Override
  public void drawString(AttributedCharacterIterator iterator, int x, int y) {
    // dummy ignores
  }

  @Override
  public boolean drawImage(Image img, int x, int y, ImageObserver observer) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public boolean drawImage(Image img, int x, int y, int width, int height, ImageObserver observer) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public boolean drawImage(Image img, int x, int y, Color bgcolor, ImageObserver observer) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public boolean drawImage(Image img, int x, int y, int width, int height, Color bgcolor, ImageObserver observer) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, ImageObserver observer) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, Color bgcolor, ImageObserver observer) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void dispose() {
    // dummy ignores
  }

  @Override
  public void drawString(AttributedCharacterIterator iterator, float x, float y) {
    // dummy ignores
  }

  @Override
  public void drawGlyphVector(GlyphVector g, float x, float y) {
    // dummy ignores
  }

  @Override
  public boolean hit(Rectangle rect, Shape s, boolean onStroke) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public GraphicsConfiguration getDeviceConfiguration() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void setComposite(Composite comp) {
    // dummy ignores
  }

  @Override
  public void setPaint(Paint paint) {
    // dummy ignores
  }

  @Override
  public void setStroke(Stroke s) {
    // dummy ignores
  }

  @Override
  public void setRenderingHint(RenderingHints.Key hintKey, Object hintValue) {
    // dummy ignores
  }

  @Override
  public Object getRenderingHint(RenderingHints.Key hintKey) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void setRenderingHints(Map<?, ?> hints) {
    // dummy ignores
  }

  @Override
  public void addRenderingHints(Map<?, ?> hints) {
    // dummy ignores
  }

  @Override
  public RenderingHints getRenderingHints() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public Graphics create() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void translate(int x, int y) {
    // dummy ignores
  }

  @Override
  public Color getColor() {
    return this.color;
  }

  @Override
  public void setColor(Color c) {
    this.color = c;
  }

  @Override
  public void setPaintMode() {
    // dummy ignores
  }

  @Override
  public void setXORMode(Color c1) {
    // dummy ignores
  }

  @Override
  public Font getFont() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void setFont(Font font) {
    // dummy ignores
  }

  @Override
  public FontMetrics getFontMetrics(Font f) {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public Rectangle getClipBounds() {
    return null;
  }

  @Override
  public void clipRect(int x, int y, int width, int height) {
    // dummy ignores
  }

  @Override
  public void setClip(int x, int y, int width, int height) {
    // dummy ignores
  }

  @Override
  public Shape getClip() {
    return null;
  }

  @Override
  public void setClip(Shape clip) {
    // dummy ignores
  }

  @Override
  public void copyArea(int x, int y, int width, int height, int dx, int dy) {
    // dummy ignores
  }

  @Override
  public void drawLine(int x1, int y1, int x2, int y2) {
    Shape shape = new Line2D.Double(x1, y1, x2, y2);
    this.draw(shape);
  }

  @Override
  public void fillRect(int x, int y, int width, int height) {
    Shape shape = new Rectangle(x, y, width, height);
    this.fill(shape);
  }

  @Override
  public void clearRect(int x, int y, int width, int height) {

  }

  @Override
  public void drawRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight) {
    Shape shape = new RoundRectangle2D.Double(x, y, width, height, arcWidth, arcHeight);
    this.draw(shape);
  }

  @Override
  public void fillRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight) {
    Shape shape = new RoundRectangle2D.Double(x, y, width, height, arcWidth, arcHeight);
    this.fill(shape);
  }

  @Override
  public void drawOval(int x, int y, int width, int height) {
    Shape shape = new Ellipse2D.Double(x, y, width, height);
    this.draw(shape);
  }

  @Override
  public void fillOval(int x, int y, int width, int height) {
    Shape shape = new Ellipse2D.Double(x, y, width, height);
    this.fill(shape);
  }

  @Override
  public void drawArc(int x, int y, int width, int height, int startAngle, int arcAngle) {
    Shape shape = new Arc2D.Double(x, y, width, height, startAngle, arcAngle, Arc2D.OPEN);
    this.draw(shape);
  }

  @Override
  public void fillArc(int x, int y, int width, int height, int startAngle, int arcAngle) {
    Shape shape = new Arc2D.Double(x, y, width, height, startAngle, arcAngle, Arc2D.PIE);
    this.fill(shape);
  }

  @Override
  public void drawPolyline(int[] xPoints, int[] yPoints, int nPoints) {
    Path2D shape = new Path2D.Double();
    shape.moveTo(xPoints[0], yPoints[0]);
    for (int i = 1; i < nPoints; i++) {
      shape.lineTo(xPoints[i], yPoints[i]);
    }
    this.draw(shape);
  }

  @Override
  public void drawPolygon(int[] xPoints, int[] yPoints, int nPoints) {
    Shape shape = new Polygon(xPoints, yPoints, nPoints);
    this.draw(shape);
  }

  @Override
  public void fillPolygon(int[] xPoints, int[] yPoints, int nPoints) {
    Shape shape = new Polygon(xPoints, yPoints, nPoints);
    this.fill(shape);
  }

  @Override
  public void translate(double tx, double ty) {
    // dummy ignores
  }

  @Override
  public void rotate(double theta) {
    // dummy ignores
  }

  @Override
  public void rotate(double theta, double x, double y) {
    // dummy ignores
  }

  @Override
  public void scale(double sx, double sy) {
    // dummy ignores
  }

  @Override
  public void shear(double shx, double shy) {
    // dummy ignores
  }

  @Override
  public void transform(AffineTransform Tx) {
    // dummy ignores
  }

  @Override
  public void setTransform(AffineTransform Tx) {
    // dummy ignores
  }

  @Override
  public AffineTransform getTransform() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public Paint getPaint() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public Composite getComposite() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void setBackground(Color color) {
    // dummy ignores
  }

  @Override
  public Color getBackground() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public Stroke getStroke() {
    throw new UnsupportedOperationException("Not supported yet.");
  }

  @Override
  public void clip(Shape s) {
    // dummy ignores
  }

  @Override
  public FontRenderContext getFontRenderContext() {
    throw new UnsupportedOperationException("Not supported yet.");
  }
}
