package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.Quadrant;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager.ShapeTypes;

public class ShapeSpawnSphere extends ShapeSphereBlocky
{
    protected Vec3d[] quadrantCenters = new Vec3d[4];
    protected double margin = 1.5;

    public ShapeSpawnSphere(ShapeTypes shape, Color4f color, double radius)
    {
        super(shape, color, radius);
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

        lines.add(StringUtils.translate("minihud.gui.label.margin_value", String.format("%.2f", this.margin)));

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
    protected boolean isPositionOnOrInsideSphere(int x, int y, int z, EnumFacing outSide)
    {
        final double maxDistSq = this.radiusSq;
        Vec3d quadrantCenter = this.quadrantCenters[Quadrant.getQuadrant(x, z, this.effectiveCenter).ordinal()];
        double dx = x + 0.5;
        double dy = y + 1;
        double dz = z + 0.5;

        return quadrantCenter.squareDistanceTo(dx, dy, dz) < maxDistSq || this.effectiveCenter.squareDistanceTo(dx, dy, dz) < maxDistSq;
    }

    @Override
    protected boolean isAdjacentPositionOutside(BlockPos pos, EnumFacing dir)
    {
        final double maxDistSq = this.radiusSq;
        Vec3d quadrantCenter = this.quadrantCenters[Quadrant.getQuadrant(pos.getX(), pos.getZ(), this.effectiveCenter).ordinal()];
        double x = pos.getX() + dir.getXOffset() + 0.5;
        double y = pos.getY() + dir.getYOffset() + 1;
        double z = pos.getZ() + dir.getZOffset() + 0.5;

        return quadrantCenter.squareDistanceTo(x, y, z) >= maxDistSq && this.effectiveCenter.squareDistanceTo(x, y, z) >= maxDistSq;
    }
}
