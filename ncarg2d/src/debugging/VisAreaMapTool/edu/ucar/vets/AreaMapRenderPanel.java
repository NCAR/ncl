
package edu.ucar.vets;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Line2D;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JPanel;

/**
 *
 * @author brownrig
 *
 * A JPanel where the rendering of the AreaMap occurs.
 *
 */
public class AreaMapRenderPanel extends JPanel {

    public AreaMapRenderPanel() {
        super();
        ViewEventManager vem = new ViewEventManager();
        addMouseListener(vem);
        addMouseMotionListener(vem);
        addMouseWheelListener(vem);
    }

    public void setAreaMap(AreaMap areaMap) {
        this.areaMap = areaMap;
        if (this.areaMap == null)
            return;

        // find its extent...
        amMinX = amMinY = Integer.MAX_VALUE;
        amMaxX = amMaxY = -Integer.MAX_VALUE;
        AreaMap.Node n = this.areaMap.getFirstNode();
        while (!areaMap.isLastNode(n.index)) {
            if (n.x < amMinX) amMinX = n.x;
            if (n.y < amMinY) amMinY = n.y;
            if (n.x > amMaxX) amMaxX = n.x;
            if (n.y > amMaxY) amMaxY = n.y;

            n = this.areaMap.getNode(n.nextNode);
        }

        System.out.println(amMinX + " -- " + amMaxX);
        System.out.println(amMinY + " -- " + amMaxY);

        viewMinX = amMinX;
        viewMinY = amMinY;
        viewMaxX = amMaxX;
        viewMaxY = amMaxY;
    }

    public void setHighlightEdgeId(int id) {
        highlightEdge = id;
        repaint();
    }

    public void addSelectListener(SelectListener l) {
        listeners.add(l);
    }

    @Override
    public void paintComponent(Graphics g) {
        if (areaMap == null) return;
        
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;

        int viewWidth = viewMaxX - viewMinX;
        int viewHeight = viewMaxY - viewMinY;

        int width = getWidth();
        int height = getHeight();
        g2d.setBackground(Color.white);
        g2d.clearRect(0, 0, width, height);

        int prevX = -1;
        int prevY = -1;

        g2d.setColor(Color.black);
        AreaMap.Node n = areaMap.getFirstNode();
        while (!areaMap.isLastNode(n.index)) {
            int x = (n.x - viewMinX) * width / viewWidth;
            int y = height - (n.y - viewMinY) * height / viewHeight;
            if (n.groupId != 0) {
                g2d.drawLine(prevX, prevY, x, y);
            }
            prevX = x;
            prevY = y;

            n = areaMap.getNode(n.nextNode);
        }

        if (highlightEdge > 0) {
            g2d.setColor(Color.red);
            n = areaMap.getNode(highlightEdge);
            AreaMap.Node p = areaMap.getNode(n.prevNode);
            int x1 = (p.x - viewMinX) * width / viewWidth;
            int y1 = height - (p.y - viewMinY) * height / viewHeight;
            int x2 = (n.x - viewMinX) * width / viewWidth;
            int y2 = height - (n.y - viewMinY) * height / viewHeight;
            g2d.drawLine(x1, y1, x2, y2);
            g2d.setColor(Color.cyan);
            g2d.fillOval(x2-3, y2-3, 6, 6);
        }
    }

    private void fireSelectionEvent(int nodeId) {
        for (SelectListener l : listeners) {
            l.selectNode(nodeId);
        }
    }


    private class ViewEventManager extends MouseAdapter implements MouseWheelListener {

        public void mousePressed(MouseEvent evt) {
            startX = evt.getX();
            startY = evt.getY();
            startingWidth = viewMaxX - viewMinX;
            viewCenterX = (viewMinX + viewMaxX) / 2;
            viewCenterY = (viewMinY + viewMaxY) / 2;
        }
        
        public void mouseDragged(MouseEvent evt) {
            int mods = evt.getModifiersEx();
            if ((mods & MouseEvent.BUTTON1_DOWN_MASK) != 0 &&
                    (mods & MouseEvent.SHIFT_DOWN_MASK) == 0)
            {
                int panFactor = (viewMaxX - viewMinX) / getWidth();
                int offsetX = panFactor * (evt.getX() - startX);
                int offsetY = panFactor * (evt.getY() - startY);
                viewMinX -= offsetX;
                viewMaxX -= offsetX;
                viewMinY += offsetY;
                viewMaxY += offsetY;
                repaint();
                
                startX = evt.getX();
                startY = evt.getY();
            }

            else if ((mods & MouseEvent.BUTTON1_DOWN_MASK) != 0 &&
                    (mods & MouseEvent.SHIFT_DOWN_MASK) != 0)
            {
                int numPixMoved = evt.getY() - startY;
                zoomView(numPixMoved);
            }
        }

        public void mouseClicked(MouseEvent evt) {
            int x = evt.getX();
            int y = evt.getY();
            int width = getWidth();
            int height = getHeight();
            int viewWidth = viewMaxX - viewMinX;
            int viewHeight = viewMaxY - viewMinY;
            
            int prevX = -1;
            int prevY = -1;
            int pickedSegId = -1;
            AreaMap.Node n = areaMap.getFirstNode();
            while (!areaMap.isLastNode(n.index)) {
                int nextX = (n.x - viewMinX) * width / viewWidth;
                int nextY = height - (n.y - viewMinY) * height / viewHeight;
                if (n.groupId != 0) {
                    // test if we can't trivially exclude...
                    if (!(nextX < 0 && prevX < 0 || (nextX > width && prevX > width)
                            || (nextY < 0 && prevY < 0) || (nextY > height && prevY > height))) {
                        Line2D.Double lineSeg = new Line2D.Double(prevX, prevY, nextX, nextY);
                        if (Math.abs(lineSeg.ptSegDist(x, y)) < PICK_TOLERANCE) {
                            pickedSegId = n.index;
                            break;
                        }
                    }
                }
                prevX = nextX;
                prevY = nextY;

                n = areaMap.getNode(n.nextNode);
            }

            fireSelectionEvent(pickedSegId);
            repaint();
        }

        public void mouseWheelMoved(MouseWheelEvent evt) {
            startingWidth = viewMaxX - viewMinX;
            viewCenterX = (viewMinX + viewMaxX) / 2;
            viewCenterY = (viewMinY + viewMaxY) / 2;
            zoomView(evt.getWheelRotation() * AMP_FACTOR);
        }

        private void zoomView(int amount) {
            float zoom = (float) (amount) * startingWidth / getWidth();
            viewMinX = viewCenterX - ((int) (startingWidth + zoom) / 2);
            viewMaxX = viewCenterX + ((int) (startingWidth + zoom) / 2);
            viewMinY = viewCenterY - ((int) (startingWidth + zoom) / 2);
            viewMaxY = viewCenterY + ((int) (startingWidth + zoom) / 2);
            repaint();
        }
        
        int startX;
        int startY;
        int startingWidth;
        int viewCenterX;
        int viewCenterY;
        final int AMP_FACTOR = 40; // zoom-motion amplification
        final double PICK_TOLERANCE = 3.0;
    }

    private List<SelectListener> listeners = new ArrayList<SelectListener>();

    private int highlightEdge = -1;

    private AreaMap areaMap;
    private int amMinX;
    private int amMinY;
    private int amMaxX;
    private int amMaxY;

    private int viewMinX;
    private int viewMinY;
    private int viewMaxX;
    private int viewMaxY;
}
