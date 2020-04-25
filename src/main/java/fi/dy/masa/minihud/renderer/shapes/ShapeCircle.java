package fi.dy.masa.minihud.renderer.shapes;

import java.util.HashSet;
import java.util.List;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;
import fi.dy.masa.minihud.util.ShapeRenderType;

public class ShapeCircle extends ShapeCircleBase
{
    protected int height = 1;

    public ShapeCircle()
    {
        super(ShapeType.CIRCLE, Configs.Colors.SHAPE_CIRCLE.getColor(), 16);
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        this.renderCircleShape(cameraPos);
        this.onPostUpdate(entity.getPos());
    }

    public int getHeight()
    {
        return this.height;
    }

    public void setHeight(int height)
    {
        this.height = MathHelper.clamp(height, 1, 260);
        this.setNeedsUpdate();
    }

    protected void renderCircleShape(Vec3d cameraPos)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        BlockPos posCenter = this.getCenterBlock();
        BlockPos.Mutable posMutable = new BlockPos.Mutable();
        HashSet<BlockPos> circlePositions = new HashSet<>();

        Direction.Axis axis = this.mainAxis.getAxis();

        for (int i = 0; i < this.height; ++i)
        {
            posMutable.set(posCenter.getX() + this.mainAxis.getOffsetX() * i,
                           posCenter.getY() + this.mainAxis.getOffsetY() * i,
                           posCenter.getZ() + this.mainAxis.getOffsetZ() * i);

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
        Direction[] sides = FACING_ALL;

        // Exclude the two sides on the main axis
        if (this.renderType != ShapeRenderType.FULL_BLOCK)
        {
            sides = new Direction[4];

            for (int i = 0, index = 0; i < 6; ++i)
            {
                Direction side = FACING_ALL[i];

                if (side.getAxis() != mainAxis.getAxis())
                {
                    sides[index++] = side;
                }
            }
        }

        this.renderPositions(circlePositions, sides, mainAxis, this.color, cameraPos);

        BUFFER_1.end();

        renderQuads.uploadData(BUFFER_1);
    }

    @Override
    protected boolean isPositionOnOrInsideRing(int blockX, int blockY, int blockZ, Direction outSide, Direction mainAxis)
    {
        Direction.Axis axis = mainAxis.getAxis();

        double x = axis == Direction.Axis.X ? this.effectiveCenter.x : (double) blockX + 0.5;
        double y = axis == Direction.Axis.Y ? this.effectiveCenter.y : (double) blockY + 0.5;
        double z = axis == Direction.Axis.Z ? this.effectiveCenter.z : (double) blockZ + 0.5;
        double dist = this.effectiveCenter.squaredDistanceTo(x, y, z);
        double diff = this.radiusSq - dist;

        if (diff > 0)
        {
            return true;
        }

        double xAdj = axis == Direction.Axis.X ? this.effectiveCenter.x : (double) blockX + outSide.getOffsetX() + 0.5;
        double yAdj = axis == Direction.Axis.Y ? this.effectiveCenter.y : (double) blockY + outSide.getOffsetY() + 0.5;
        double zAdj = axis == Direction.Axis.Z ? this.effectiveCenter.z : (double) blockZ + outSide.getOffsetZ() + 0.5;
        double distAdj = this.effectiveCenter.squaredDistanceTo(xAdj, yAdj, zAdj);
        double diffAdj = this.radiusSq - distAdj;

        return diffAdj > 0 && Math.abs(diff) < Math.abs(diffAdj);
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();

        String aq = GuiBase.TXT_AQUA;
        String gl = GuiBase.TXT_GOLD;
        String gr = GuiBase.TXT_GRAY;
        String rst = GuiBase.TXT_GRAY;

        lines.add(2, gr + StringUtils.translate("minihud.gui.label.height_value", gl + this.getHeight() + rst));
        lines.add(3, gr + StringUtils.translate("minihud.gui.label.circle.main_axis_value",
                aq + org.apache.commons.lang3.StringUtils.capitalize(this.getMainAxis().toString().toLowerCase()) + rst));

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
