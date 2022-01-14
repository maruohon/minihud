package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.Quadrant;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.shape.SphereUtils;

public class ShapeSpawnSphere extends ShapeSphereBlocky
{
    protected Vec3d[] quadrantCenters;
    protected double margin = 0.0;

    public ShapeSpawnSphere()
    {
        this(ShapeType.ADJUSTABLE_SPAWN_SPHERE, Configs.Colors.SHAPE_ADJUSTABLE_SPAWN_SPHERE.getColor(), 24.0);
    }

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
        Vec3d center = this.getEffectiveCenter();

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

        lines.add(2, StringUtils.translate("minihud.gui.hover.shape.margin_value", d2(this.margin)));

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

        this.setMargin(JsonUtils.getDouble(obj, "margin"));
    }

    @Override
    protected SphereUtils.RingPositionTest getPositionTest()
    {
        return this::isPositionOnOrInsideRing;
    }

    protected boolean isPositionOnOrInsideRing(int x, int y, int z, Direction outSide)
    {
        final double maxDistSq = this.getSquaredRadius();
        Vec3d effectiveCenter = this.getEffectiveCenter();
        Vec3d quadrantCenter = this.quadrantCenters[Quadrant.getQuadrant(x, z, effectiveCenter).ordinal()];
        double dx = x + 0.5;
        double dy = y + 1;
        double dz = z + 0.5;

        return quadrantCenter.squaredDistanceTo(dx, dy, dz) < maxDistSq ||
               effectiveCenter.squaredDistanceTo(dx, dy, dz) < maxDistSq;
    }
}
