import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

class Day18 {
    public static void main(String[] args) {
        try (var reader = new BufferedReader(new FileReader("input"))) {
            var cubes = new HashSet<Vector3>();
            for (String line; (line = reader.readLine()) != null;) {
                String[] xyz = line.split(",");
                int x = Integer.valueOf(xyz[0]);
                int y = Integer.valueOf(xyz[1]);
                int z = Integer.valueOf(xyz[2]);
                cubes.add(new Vector3(x, y, z));
            }

            var droplet = new LavaDroplet(cubes);
            System.out.println("Part One: " + droplet.surfaceArea());

            var steam = new Steam(cubes);
            System.out.println("Part Two: " + steam.surfaceArea());
        } catch (IOException | NumberFormatException e) {
            System.err.println("Failed to read input: " + e.getMessage());
        }
    }
}

record Vector3(
    int x,
    int y,
    int z
) {
    public Vector3 min(Vector3 other) {
        int _x = Math.min(x, other.x);
        int _y = Math.min(y, other.y);
        int _z = Math.min(z, other.z);
        return new Vector3(_x, _y, _z);
    }

    public Vector3 max(Vector3 other) {
        int _x = Math.max(x, other.x);
        int _y = Math.max(y, other.y);
        int _z = Math.max(z, other.z);
        return new Vector3(_x, _y, _z);
    }
}

class BoundingBox {

    private final Vector3 min;
    private final Vector3 max;

    public BoundingBox(Iterable<Vector3> cubes) {
        var minPoint = new Vector3(Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE);
        var maxPoint = new Vector3(Integer.MIN_VALUE, Integer.MIN_VALUE, Integer.MIN_VALUE);
        for (Vector3 cube : cubes) {
            minPoint = minPoint.min(cube);
            maxPoint = maxPoint.max(cube);
        }
        this.min = minPoint;
        this.max = maxPoint;
    }

    public BoundingBox(Vector3 minPoint, Vector3 maxPoint) {
        this.min = minPoint;
        this.max = maxPoint;
    }

    public boolean contains(Vector3 point) {
        return min.x() <= point.x() && point.x() <= max.x()
            && min.y() <= point.y() && point.y() <= max.y()
            && min.z() <= point.z() && point.z() <= max.z();
    }

    public Vector3 minPoint() {
        return min;
    }

    public Vector3 maxPoint() {
        return max;
    }

    public int width() {
        return max.x() - min.x() + 1;
    }

    public int height() {
        return max.y() - min.y() + 1;
    }

    public int depth() {
        return max.z() - min.z() + 1;
    }

    @Override
    public String toString() {
        return "BoundingBox { " + min + ", " + max + " }";
    }
}

interface Volume {

    // Returns at least one point for every disjoint volume region. Implementations might also
    // return multiple point for the same region.
    Collection<Vector3> disjointPoints();

    boolean containsPoint(int x, int y, int z);

    default int surfaceArea() {
        var points = new HashSet<Vector3>();
        points.addAll(disjointPoints());

        int area = 0;
        while (!points.isEmpty()) {
            var visited = new HashSet<Vector3>();
            Vector3 point = points.iterator().next();
            area += surfaceAreaImpl(visited, point.x(), point.y(), point.z());
            points.removeAll(visited);
        }
        return area;
    }

    private int surfaceAreaImpl(HashSet<Vector3> visited, int x, int y, int z) {
        if (!containsPoint(x, y, z)) {
            return 0;
        }
        if (!visited.add(new Vector3(x, y, z))) {
            return 0;
        }
        int area = 6;
        if (containsPoint(x - 1, y, z)) {
            area += surfaceAreaImpl(visited, x - 1, y, z) - 1;
        }
        if (containsPoint(x + 1, y, z)) {
            area += surfaceAreaImpl(visited, x + 1, y, z) - 1;
        }
        if (containsPoint(x, y - 1, z)) {
            area += surfaceAreaImpl(visited, x, y - 1, z) - 1;
        }
        if (containsPoint(x, y + 1, z)) {
            area += surfaceAreaImpl(visited, x, y + 1, z) - 1;
        }
        if (containsPoint(x, y, z - 1)) {
            area += surfaceAreaImpl(visited, x, y, z - 1) - 1;
        }
        if (containsPoint(x, y, z + 1)) {
            area += surfaceAreaImpl(visited, x, y, z + 1) - 1;
        }
        return area;
    }
}

class LavaDroplet implements Volume {

    private final HashSet<Vector3> cubes;

    public LavaDroplet(Collection<Vector3> droplet) {
        this.cubes = new HashSet<>();
        this.cubes.addAll(droplet);
    }

    @Override
    public Collection<Vector3> disjointPoints() {
        return cubes;
    }

    @Override
    public boolean containsPoint(int x, int y, int z) {
        return cubes.contains(new Vector3(x, y, z));
    }
}

class Steam implements Volume {

    private final HashSet<Vector3> cubes;
    private final BoundingBox bbox;

    public Steam(Collection<Vector3> droplet) {
        this.cubes = new HashSet<>();
        this.cubes.addAll(droplet);

        var bounds = new BoundingBox(droplet);
        Vector3 minPoint = bounds.minPoint();
        minPoint = new Vector3(minPoint.x() - 1, minPoint.y() - 1, minPoint.z() - 1);
        Vector3 maxPoint = bounds.maxPoint();
        maxPoint = new Vector3(maxPoint.x() + 1, maxPoint.y() + 1, maxPoint.z() + 1);
        this.bbox = new BoundingBox(minPoint, maxPoint);
    }

    @Override
    public Collection<Vector3> disjointPoints() {
        return List.of(bbox.minPoint());
    }

    @Override
    public boolean containsPoint(int x, int y, int z) {
        var v = new Vector3(x, y, z);
        return bbox.contains(v) && !cubes.contains(v);
    }

    @Override
    public int surfaceArea() {
        int w = bbox.width();
        int h = bbox.height();
        int d = bbox.depth();
        return Volume.super.surfaceArea() - 2 * (w * h + w * d + h * d);
    }
}
