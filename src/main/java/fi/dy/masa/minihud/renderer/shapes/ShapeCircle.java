package fi.dy.masa.minihud.renderer.shapes;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import it.unimi.dsi.fastutil.longs.Long2ByteOpenHashMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;
import fi.dy.masa.minihud.renderer.RenderUtils;
import fi.dy.masa.minihud.util.ShapeRenderType;
import fi.dy.masa.minihud.util.shape.SphereUtils;

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
        this.needsUpdate = false;
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
        Vec3d effectiveCenter = this.getEffectiveCenter();
        Direction.Axis axis = this.mainAxis.getAxis();
        double expand = 0;

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        if (this.getCombineQuads())
        {
            mutablePos.set(effectiveCenter.x, effectiveCenter.y, effectiveCenter.z);

            if (axis == Direction.Axis.Y)
            {
                SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);
            }
            else
            {
                SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, this.mainAxis, test);
            }

            Long2ObjectOpenHashMap<SideQuad> strips =
                    SphereUtils.buildSphereShellToStrips(positions, axis, test, this.renderType, this.layerRange);
            List<SideQuad> quads = buildStripsToQuadsForCircle(strips, this.mainAxis, this.height);

            RenderUtils.renderQuads(quads, this.color, expand, cameraPos, BUFFER_1);
        }
        else
        {
            BlockPos posCenter = BlockPos.ofFloored(effectiveCenter);
            int offX = this.mainAxis.getOffsetX();
            int offY = this.mainAxis.getOffsetY();
            int offZ = this.mainAxis.getOffsetZ();
    
            for (int i = 0; i < this.height; ++i)
            {
                mutablePos.set(posCenter.getX() + offX * i,
                               posCenter.getY() + offY * i,
                               posCenter.getZ() + offZ * i);
    
                if (axis == Direction.Axis.Y)
                {
                    SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);
                }
                else
                {
                    SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, this.mainAxis, test);
                }
            }

            Direction[] sides = this.getSides();
            RenderUtils.renderCircleBlockPositions(positions, sides, test, this.renderType, this.layerRange,
                                                   this.color, expand, cameraPos, BUFFER_1);
        }

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

        Vec3d effectiveCenter = this.getEffectiveCenter();
        double radiusSq = this.getSquaredRadius();
        double x = axis == Direction.Axis.X ? effectiveCenter.x : (double) blockX + 0.5;
        double y = axis == Direction.Axis.Y ? effectiveCenter.y : (double) blockY + 0.5;
        double z = axis == Direction.Axis.Z ? effectiveCenter.z : (double) blockZ + 0.5;
        double distSq = effectiveCenter.squaredDistanceTo(x, y, z);
        double diff = radiusSq - distSq;

        if (diff > 0)
        {
            return true;
        }

        double xAdj = x + outSide.getOffsetX();
        double yAdj = y + outSide.getOffsetY();
        double zAdj = z + outSide.getOffsetZ();
        double distAdjSq = effectiveCenter.squaredDistanceTo(xAdj, yAdj, zAdj);
        double diffAdj = radiusSq - distAdjSq;

        return diffAdj > 0 && Math.abs(diff) < Math.abs(diffAdj);
    }

    public static List<SideQuad> buildStripsToQuadsForCircle(Long2ObjectOpenHashMap<SideQuad> strips,
                                                                         Direction mainAxisDirection, int circleHeight)
    {
        List<SideQuad> quads = new ArrayList<>();
        Long2ByteOpenHashMap handledPositions = new Long2ByteOpenHashMap();
        final Direction.Axis mainAxis = mainAxisDirection.getAxis();

        for (SideQuad strip : strips.values())
        {
            final long pos = strip.startPos();
            final Direction side = strip.side();

            if (SphereUtils.isHandledAndMarkHandled(pos, side, handledPositions))
            {
                continue;
            }

            final long startPos = side == mainAxisDirection ? SphereUtils.offsetPos(pos, mainAxisDirection, circleHeight - 1) : pos;
            final int height = side.getAxis() != mainAxis ? circleHeight : strip.height();

            quads.add(new SideQuad(startPos, strip.width(), height, side));
        }

        return quads;
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();

        lines.add(2, StringUtils.translate("minihud.gui.hover.shape.circle.main_axis_value",
                org.apache.commons.lang3.StringUtils.capitalize(this.getMainAxis().toString().toLowerCase())));
        lines.add(3, StringUtils.translate("minihud.gui.hover.shape.height_value", this.getHeight()));

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
