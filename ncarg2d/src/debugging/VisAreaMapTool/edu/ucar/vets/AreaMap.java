
package edu.ucar.vets;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 *
 * @author brownrig
 *
 * A class to read and represent an AreaMap.
 *
 */
public class AreaMap {

    public class Node {

        public Node(AreaMap map, int index) {
            this.index = index;
            flag = map.areaMap[index];
            x = map.areaMap[index + 1];
            y = map.areaMap[index + 2];
            nextNode = map.areaMap[index + 3];
            prevNode = map.areaMap[index + 4];
            nextCoord = map.areaMap[index + 5];
            prevCoord = map.areaMap[index + 6];
            groupId = map.areaMap[index + 7];
            leftId = map.areaMap[index + 8];
            rightId = map.areaMap[index + 9];
        }

        final public int index;
        final public int flag;
        final public int x;
        final public int y;
        final public int nextNode;
        final public int prevNode;
        final public int nextCoord;
        final public int prevCoord;
        final public int groupId;
        final public int leftId;
        final public int rightId;
    }


    public static AreaMap readAreaMap(String areaMapFile) throws IOException {
        BufferedReader inp = new BufferedReader(
                new InputStreamReader(
                    new FileInputStream(areaMapFile)));
        String line = null;

        try {
            AreaMap me = new AreaMap();
            line = inp.readLine();
            StringTokenizer parser = new StringTokenizer(line);
            parser.nextElement();
            int numRecs = Integer.parseInt(parser.nextToken());
            me.areaMap = new int[numRecs+1];
            me.areaMap[1] = numRecs;

            while ((line = inp.readLine()) != null) {
                parser = new StringTokenizer(line);
                int index = Integer.parseInt(parser.nextToken());
                int value = Integer.parseInt(parser.nextToken());
                me.areaMap[index] = value;
            }

            return me;

        } catch (Exception ex) {
            System.err.println(ex.toString());
            throw new IOException("Error parsing AreaMap: " + line);
        }
    }

    public Node getNode(int index) {
        return new Node(this, index);
    }

    public Node getFirstNode() {
        return getNode(areaMap[11]);
    }

    public Node getLastNode() {
        return getNode(areaMap[21]);
    }

    public boolean isFirstNode(int index) {
        return (index == 8);
    }

    public boolean isLastNode(int index) {
        return (index == 18);
    }

    private int[] areaMap;
}
