package minihud.renderer.shapes;

import java.util.HashSet;
import java.util.List;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import net.minecraft.entity.Entity;

import malilib.util.MathUtils;
import malilib.util.StringUtils;
import malilib.util.data.json.JsonUtils;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.position.BlockPos;
import malilib.util.position.Direction;
import malilib.util.position.Vec3d;
import minihud.config.Configs;
import minihud.util.value.ShapeRenderType;

public class ShapeCircle extends ShapeCircleBase
{
    protected int height = 1;

    public ShapeCircle()
    {
        super(ShapeType.CIRCLE, Configs.Colors.SHAPE_CIRCLE.getColor(), 16);
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        this.renderCircleShape(cameraPos);
        this.onPostUpdate(EntityWrap.getEntityPos(entity));
    }

    public int getHeight()
    {
        return this.height;
    }

    public void setHeight(int height)
    {
        this.height = MathUtils.clamp(height, 1, 260);
        this.setNeedsUpdate();
    }

    protected void renderCircleShape(Vec3d cameraPos)
    {
        BlockPos posCenter = this.getCenterBlock();
        BlockPos.MutBlockPos posMutable = new BlockPos.MutBlockPos();
        HashSet<BlockPos> circlePositions = new HashSet<>();

        Direction.Axis axis = this.mainAxis.getAxis();

        for (int i = 0; i < this.height; ++i)
        {
            posMutable.set(posCenter.getX() + this.mainAxis.getXOffset() * i,
                           posCenter.getY() + this.mainAxis.getYOffset() * i,
                           posCenter.getZ() + this.mainAxis.getZOffset() * i);

            if (axis == Direction.Axis.Y)
            {
                this.addPositionsOnHorizontalRing(circlePositions, posMutable, Direction.NORTH);
            }
            else
            {
                this.addPositionsOnVerticalRing(circlePositions, posMutable, Direction.UP, this.mainAxis);
            }
        }

        Direction mainAxis = this.mainAxis;
        Direction[] sides = Direction.ALL_DIRECTIONS;

        // Exclude the two sides on the main axis
        if (this.renderType != ShapeRenderType.FULL_BLOCK)
        {
            sides = new Direction[4];

            for (int i = 0, index = 0; i < 6; ++i)
            {
                Direction side = Direction.ALL_DIRECTIONS[i];

                if (side.getAxis() != mainAxis.getAxis())
                {
                    sides[index++] = side;
                }
            }
        }

        this.startBuffers();
        this.renderPositions(circlePositions, sides, mainAxis, this.color, cameraPos);
        this.uploadBuffers();
    }

    @Override
    protected boolean isPositionOnOrInsideRing(int blockX, int blockY, int blockZ, Direction outSide, Direction mainAxis)
    {
        Direction.Axis axis = mainAxis.getAxis();

        double x = axis == Direction.Axis.X ? this.effectiveCenter.x : (double) blockX + 0.5;
        double y = axis == Direction.Axis.Y ? this.effectiveCenter.y : (double) blockY + 0.5;
        double z = axis == Direction.Axis.Z ? this.effectiveCenter.z : (double) blockZ + 0.5;
        double dist = this.effectiveCenter.squareDistanceTo(x, y, z);
        double diff = this.radiusSq - dist;

        if (diff > 0)
        {
            return true;
        }

        double xAdj = axis == Direction.Axis.X ? this.effectiveCenter.x : (double) blockX + outSide.getXOffset() + 0.5;
        double yAdj = axis == Direction.Axis.Y ? this.effectiveCenter.y : (double) blockY + outSide.getYOffset() + 0.5;
        double zAdj = axis == Direction.Axis.Z ? this.effectiveCenter.z : (double) blockZ + outSide.getZOffset() + 0.5;
        double distAdj = this.effectiveCenter.squareDistanceTo(xAdj, yAdj, zAdj);
        double diffAdj = this.radiusSq - distAdj;

        return diffAdj > 0 && Math.abs(diff) < Math.abs(diffAdj);
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();

        String axis = org.apache.commons.lang3.StringUtils.capitalize(this.getMainAxis().toString().toLowerCase());
        lines.add(2, StringUtils.translate("minihud.hover.shape.height", this.getHeight()));
        lines.add(3, StringUtils.translate("minihud.hover.shape.circle.main_axis", axis));

        return lines;
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();

        obj.add("height", new JsonPrimitive(this.height));

        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        this.setHeight(JsonUtils.getInteger(obj, "height"));
    }
}
