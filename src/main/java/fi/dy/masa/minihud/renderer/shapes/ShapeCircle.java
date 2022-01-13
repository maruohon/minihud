package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
import java.util.function.Consumer;
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
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;
import fi.dy.masa.minihud.util.ShapeRenderType;
import fi.dy.masa.minihud.util.shape.SphereUtils;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

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
        this.height = MathHelper.clamp(height, 1, 8192);
        this.setNeedsUpdate();
    }

    protected void renderCircleShape(Vec3d cameraPos)
    {
        LongOpenHashSet positions = new LongOpenHashSet();
        Consumer<BlockPos.Mutable> positionConsumer = this.getPositionCollector(positions);
        SphereUtils.RingPositionTest test = this::isPositionOnOrInsideRing;
        BlockPos.Mutable mutablePos = new BlockPos.Mutable();
        Vec3d effectiveCenter = this.effectiveCenter;
        Direction.Axis axis = this.mainAxis.getAxis();
        /*
        BlockPos posCenter = new BlockPos(effectiveCenter);

        for (int i = 0; i < this.height; ++i)
        {
            mutablePos.set(posCenter.getX() + this.mainAxis.getOffsetX() * i,
                           posCenter.getY() + this.mainAxis.getOffsetY() * i,
                           posCenter.getZ() + this.mainAxis.getOffsetZ() * i);

            if (axis == Direction.Axis.Y)
            {
                SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test, this.radius);
            }
            else
            {
                SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, this.mainAxis, test, this.radius);
            }
        }
        */

        mutablePos.set(effectiveCenter.x, effectiveCenter.y, effectiveCenter.z);

        if (axis == Direction.Axis.Y)
        {
            SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test, this.radius);
        }
        else
        {
            SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, this.mainAxis, test, this.radius);
        }

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        //Direction[] sides = this.getSides();
        //this.renderPositions(positions, sides, test, this.color, 0, cameraPos);
        List<SphereUtils.SideQuad> quads = SphereUtils.buildSphereShellToQuads(positions, axis, test,
                                                                               this.renderType, this.layerRange);
        this.renderQuads(quads, this.color, 0, cameraPos);

        BUFFER_1.end();
        renderQuads.uploadData(BUFFER_1);
    }

    protected Direction[] getSides()
    {
        // Exclude the two sides on the main axis
        if (this.renderType != ShapeRenderType.FULL_BLOCK)
        {
            return SphereUtils.getDirectionsNotOnAxis(this.mainAxis.getAxis());
        }

        return PositionUtils.ALL_DIRECTIONS;
    }

    protected boolean isPositionOnOrInsideRing(int blockX, int blockY, int blockZ, Direction outSide)
    {
        Direction.Axis axis = this.mainAxis.getAxis();

        double x = axis == Direction.Axis.X ? this.effectiveCenter.x : (double) blockX + 0.5;
        double y = axis == Direction.Axis.Y ? this.effectiveCenter.y : (double) blockY + 0.5;
        double z = axis == Direction.Axis.Z ? this.effectiveCenter.z : (double) blockZ + 0.5;
        double distSq = this.effectiveCenter.squaredDistanceTo(x, y, z);
        double diff = this.radiusSq - distSq;

        if (diff > 0)
        {
            return true;
        }

        double xAdj = x + outSide.getOffsetX();
        double yAdj = y + outSide.getOffsetY();
        double zAdj = z + outSide.getOffsetZ();
        double distAdjSq = this.effectiveCenter.squaredDistanceTo(xAdj, yAdj, zAdj);
        double diffAdj = this.radiusSq - distAdjSq;

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
