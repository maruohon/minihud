package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
import java.util.function.LongConsumer;
import com.google.gson.JsonObject;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Box;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.BlockSnap;
import fi.dy.masa.malilib.util.IntBoundingBox;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;
import fi.dy.masa.minihud.renderer.RenderUtils;
import fi.dy.masa.minihud.util.RayTracer;
import fi.dy.masa.minihud.util.shape.SphereUtils;
import it.unimi.dsi.fastutil.longs.Long2ByteOpenHashMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public class ShapeLineBlock extends ShapeBlocky
{
    protected Vec3d startPos = Vec3d.ZERO;
    protected Vec3d endPos = Vec3d.ZERO;
    protected Vec3d effectiveStartPos = Vec3d.ZERO;
    protected Vec3d effectiveEndPos = Vec3d.ZERO;

    public ShapeLineBlock()
    {
        super(ShapeType.BLOCK_LINE, Configs.Colors.SHAPE_LINE_BLOCKY.getColor());

        this.setBlockSnap(BlockSnap.CENTER);
    }

    public Vec3d getStartPos()
    {
        return this.effectiveStartPos;
    }

    public Vec3d getEndPos()
    {
        return this.effectiveEndPos;
    }

    public void setStartPos(Vec3d startPos)
    {
        this.startPos = startPos;
        this.updateEffectivePositions();
    }

    public void setEndPos(Vec3d endPos)
    {
        this.endPos = endPos;
        this.updateEffectivePositions();
    }

    @Override
    public void setBlockSnap(BlockSnap snap)
    {
        super.setBlockSnap(snap);
        this.updateEffectivePositions();
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        this.renderLineShape(cameraPos);
        this.needsUpdate = false;
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();
        Vec3d s = this.startPos;
        Vec3d e = this.endPos;

        lines.add(StringUtils.translate("minihud.gui.label.shape.line.start", d2(s.x), d2(s.y), d2(s.z)));
        lines.add(StringUtils.translate("minihud.gui.label.shape.line.end",   d2(e.x), d2(e.y), d2(e.z)));

        return lines;
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        obj.add("start", JsonUtils.vec3dToJson(this.startPos));
        obj.add("end", JsonUtils.vec3dToJson(this.endPos));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        Vec3d startPos = JsonUtils.vec3dFromJson(obj, "start");
        Vec3d endPos = JsonUtils.vec3dFromJson(obj, "end");

        if (startPos != null)
        {
            this.startPos = startPos;
        }

        if (endPos != null)
        {
            this.endPos = endPos;
        }

        this.updateEffectivePositions();
    }

    protected void updateRenderPerimeter()
    {
        double range = 512;
        double minX = Math.min(this.effectiveStartPos.getX(), this.effectiveEndPos.getX()) - range;
        double minY = Math.min(this.effectiveStartPos.getY(), this.effectiveEndPos.getY()) - range;
        double minZ = Math.min(this.effectiveStartPos.getZ(), this.effectiveEndPos.getZ()) - range;
        double maxX = Math.max(this.effectiveStartPos.getX(), this.effectiveEndPos.getX()) + range;
        double maxY = Math.max(this.effectiveStartPos.getY(), this.effectiveEndPos.getY()) + range;
        double maxZ = Math.max(this.effectiveStartPos.getZ(), this.effectiveEndPos.getZ()) + range;

        this.renderPerimeter = new Box(minX, minY, minZ, maxX, maxY, maxZ);
    }

    protected void updateEffectivePositions()
    {
        this.effectiveStartPos = this.getBlockSnappedPosition(this.startPos);
        this.effectiveEndPos = this.getBlockSnappedPosition(this.endPos);
        this.updateRenderPerimeter();
        this.setNeedsUpdate();
    }

    protected void renderLineShape(Vec3d cameraPos)
    {
        final double distance = this.effectiveEndPos.distanceTo(this.effectiveStartPos);
        final double maxDist = 10000;

        if (distance > maxDist)
        {
            return;
        }

        LongOpenHashSet positions = new LongOpenHashSet();
        RayTracer tracer = new RayTracer(this.effectiveStartPos, this.effectiveEndPos);
        double expand = 0;

        tracer.iterateAllPositions(this.getLinePositionCollector(positions));

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        if (this.getCombineQuads())
        {
            Long2ObjectOpenHashMap<SideQuad> strips = this.buildPositionsToStrips(positions, this.layerRange);
            RenderUtils.renderQuads(strips.values(), this.color, expand, cameraPos, BUFFER_1);
        }
        else
        {
            RenderUtils.renderBlockPositions(positions, this.layerRange, this.color, expand, cameraPos, BUFFER_1);
        }

        BUFFER_1.end();
        renderQuads.uploadData(BUFFER_1);
    }

    protected LongConsumer getLinePositionCollector(LongOpenHashSet positionsOut)
    {
        IntBoundingBox box = this.layerRange.getExpandedBox(this.mc.world, 0);

        LongConsumer positionCollector = (pos) -> {
            if (box.containsPos(pos))
            {
                positionsOut.add(pos);
            }
        };

        return positionCollector;
    }

    public Long2ObjectOpenHashMap<SideQuad> buildPositionsToStrips(LongOpenHashSet positions, LayerRange layerRange)
    {
        Long2ObjectOpenHashMap<SideQuad> strips = new Long2ObjectOpenHashMap<>();
        Long2ByteOpenHashMap handledPositions = new Long2ByteOpenHashMap();
        Direction[] sides = PositionUtils.ALL_DIRECTIONS;
        double lengthX = Math.abs(this.effectiveEndPos.getX() - this.effectiveStartPos.getX());
        double lengthY = Math.abs(this.effectiveEndPos.getY() - this.effectiveStartPos.getY());
        double lengthZ = Math.abs(this.effectiveEndPos.getZ() - this.effectiveStartPos.getZ());
        Direction mainAxisHor = lengthX >= lengthZ ? Direction.WEST : Direction.NORTH;
        Direction mainAxisAll = lengthY >= lengthX && lengthY >= lengthZ ? Direction.DOWN : mainAxisHor;

        for (long pos : positions)
        {
            if (layerRange.isPositionWithinRange(pos) == false)
            {
                continue;
            }

            for (Direction side : sides)
            {
                if (SphereUtils.isHandledAndMarkHandled(pos, side, handledPositions) ||
                    positions.contains(BlockPos.offset(pos, side)))
                {
                    continue;
                }

                final Direction minDir = side.getAxis().isVertical() ? mainAxisHor : mainAxisAll;
                final Direction maxDir = minDir.getOpposite();
                final int lengthMin = getStripLengthOnSide(pos, side, minDir, positions, handledPositions);
                final int lengthMax = getStripLengthOnSide(pos, side, maxDir, positions, handledPositions);
                final long startPosLong = SphereUtils.offsetPos(pos, minDir, lengthMin);
                final long index = SphereUtils.getCompressedPosSide(startPosLong, side);
                int width = lengthMin + lengthMax + 1;
                int height = 1;

                // The render method considers the width of top and bottom quads as going along the x-axis,
                // and thus the height goes along the z-axis.
                // So if the strip on the top or bottom face was built along the z-axis, then we need to swap the values.
                // And since we don't do quad merging for this line shape, we also need to swap the values
                // if the strips on the horizontal sides are going vertically.
                if ((side.getAxis().isVertical() && mainAxisHor.getAxis() == Direction.Axis.Z) ||
                    (side.getAxis().isHorizontal() && mainAxisAll.getAxis().isVertical()))
                {
                    height = width;
                    width = 1;
                }

                strips.put(index, new SideQuad(startPosLong, width, height, side));
            }
        }

        return strips;
    }

    protected static int getStripLengthOnSide(long pos,
                                              Direction side,
                                              Direction moveDirection,
                                              LongOpenHashSet positions,
                                              Long2ByteOpenHashMap handledPositions)
    {
        int length = 0;
        long adjPos = BlockPos.offset(pos, moveDirection);

        while (positions.contains(adjPos))
        {
            if (positions.contains(BlockPos.offset(adjPos, side)) ||
                SphereUtils.isHandledAndMarkHandled(adjPos, side, handledPositions))
            {
                break;
            }

            ++length;
            adjPos = BlockPos.offset(adjPos, moveDirection);
        }

        return length;
    }
}
