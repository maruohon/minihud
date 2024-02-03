package minihud.renderer.shapes;

import java.text.DecimalFormat;
import java.util.List;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import malilib.util.StringUtils;
import malilib.util.data.Color4f;
import malilib.util.data.json.JsonUtils;
import malilib.util.position.Direction;
import malilib.util.position.Quadrant;
import malilib.util.position.Vec3d;

public class ShapeSpawnSphere extends ShapeSphereBlocky
{
    protected Vec3d[] quadrantCenters;
    protected double margin = 1.5;

    public ShapeSpawnSphere(ShapeType shape, Color4f color, double radius)
    {
        super(shape, color, radius);

        this.updateQuadrantPoints();
    }

    @Override
    protected void updateEffectiveCenter()
    {
        super.updateEffectiveCenter();

        this.updateQuadrantPoints();
    }

    private void updateQuadrantPoints()
    {
        Vec3d center = this.effectiveCenter;

        if (this.quadrantCenters == null)
        {
            this.quadrantCenters = new Vec3d[4];
        }

        this.quadrantCenters[Quadrant.NORTH_WEST.ordinal()] = new Vec3d(center.x - this.margin, center.y, center.z - this.margin);
        this.quadrantCenters[Quadrant.NORTH_EAST.ordinal()] = new Vec3d(center.x + this.margin, center.y, center.z - this.margin);
        this.quadrantCenters[Quadrant.SOUTH_WEST.ordinal()] = new Vec3d(center.x - this.margin, center.y, center.z + this.margin);
        this.quadrantCenters[Quadrant.SOUTH_EAST.ordinal()] = new Vec3d(center.x + this.margin, center.y, center.z + this.margin);

        this.setNeedsUpdate();
    }

    public double getMargin()
    {
        return this.margin;
    }

    public void setMargin(double margin)
    {
        this.margin = margin;

        // Update the quadrant centers
        this.updateQuadrantPoints();
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();
        DecimalFormat fmt = new DecimalFormat("#.##");

        lines.add(2, StringUtils.translate("minihud.hover.shape.margin", fmt.format(this.margin)));

        return lines;
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        obj.add("margin", new JsonPrimitive(this.margin));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        this.margin = JsonUtils.getDouble(obj, "margin");
    }

    @Override
    protected boolean isPositionOnOrInsideRing(int x, int y, int z, Direction outSide, Direction mainAxis)
    {
        final double maxDistSq = this.radiusSq;
        Vec3d quadrantCenter = this.quadrantCenters[Quadrant.getQuadrant(x, z, this.effectiveCenter).ordinal()];
        double dx = x + 0.5;
        double dy = y + 1;
        double dz = z + 0.5;

        return quadrantCenter.squareDistanceTo(dx, dy, dz) < maxDistSq || this.effectiveCenter.squareDistanceTo(dx, dy, dz) < maxDistSq;
    }
}
