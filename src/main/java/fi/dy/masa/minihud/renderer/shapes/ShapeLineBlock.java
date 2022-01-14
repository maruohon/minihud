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
    protected BlockPos startPos = BlockPos.ORIGIN;
    protected BlockPos endPos = BlockPos.ORIGIN;

    public ShapeLineBlock()
    {
        super(ShapeType.BLOCK_LINE, Configs.Colors.SHAPE_LINE_BLOCKY.getColor());
    }

    public BlockPos getStartPos()
    {
        return this.startPos;
    }

    public BlockPos getEndPos()
    {
        return this.endPos;
    }

    public void setStartPos(BlockPos startPos)
    {
        this.startPos = startPos;
        this.setNeedsUpdate();
        this.updateRenderPerimeter();
    }

    public void setEndPos(BlockPos endPos)
    {
        this.endPos = endPos;
        this.setNeedsUpdate();
        this.updateRenderPerimeter();
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        this.renderLineShape(cameraPos, 0);
        this.needsUpdate = false;
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();
        BlockPos s = this.startPos;
        BlockPos e = this.endPos;

        lines.add(StringUtils.translate("minihud.gui.label.shape.line.start", s.getX(), s.getY(), s.getZ()));
        lines.add(StringUtils.translate("minihud.gui.label.shape.line.end",   e.getX(), e.getY(), e.getZ()));

        return lines;
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        obj.add("start", JsonUtils.blockPosToJson(this.startPos));
        obj.add("end", JsonUtils.blockPosToJson(this.endPos));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        BlockPos startPos = JsonUtils.blockPosFromJson(obj, "start");
        BlockPos endPos = JsonUtils.blockPosFromJson(obj, "end");

        if (startPos != null)
        {
            this.startPos = startPos;
        }

        if (endPos != null)
        {
            this.endPos = endPos;
        }

        this.updateRenderPerimeter();
    }

    protected void updateRenderPerimeter()
    {
        int range = 512;
        int minX = Math.min(this.startPos.getX(), this.endPos.getX()) - range;
        int minY = Math.min(this.startPos.getY(), this.endPos.getY()) - range;
        int minZ = Math.min(this.startPos.getZ(), this.endPos.getZ()) - range;
        int maxX = Math.max(this.startPos.getX(), this.endPos.getX()) + range;
        int maxY = Math.max(this.startPos.getY(), this.endPos.getY()) + range;
        int maxZ = Math.max(this.startPos.getZ(), this.endPos.getZ()) + range;

        this.renderPerimeter = new Box(minX, minY, minZ, maxX, maxY, maxZ);
    }

    protected void renderLineShape(Vec3d cameraPos, double expand)
    {
        double distance = this.endPos.getManhattanDistance(this.startPos);

        if (distance > 100000)
        {
            return;
        }

        LongOpenHashSet positions = new LongOpenHashSet();
        RayTracer tracer = new RayTracer(this.startPos, this.endPos);
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
        Direction mainAxis;
        int lengthX = Math.abs(this.endPos.getX() - this.startPos.getX());
        int lengthY = Math.abs(this.endPos.getY() - this.startPos.getY());
        int lengthZ = Math.abs(this.endPos.getZ() - this.startPos.getZ());

        if (lengthX >= lengthY && lengthX >= lengthZ)
        {
            mainAxis = Direction.WEST;
        }
        else if (lengthY >= lengthX && lengthY >= lengthZ)
        {
            mainAxis = Direction.DOWN;
        }
        else
        {
            mainAxis = Direction.NORTH;
        }
        System.out.printf("mainAxis: %s\n", mainAxis);

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

                final Direction minDir = side.getAxis() != mainAxis.getAxis() ? mainAxis : (mainAxis.getAxis().isVertical() ? Direction.WEST : Direction.DOWN);
                final Direction maxDir = minDir.getOpposite();
                final int lengthMin = getStripLengthOnSide(pos, side, minDir, positions, handledPositions);
                final int lengthMax = getStripLengthOnSide(pos, side, maxDir, positions, handledPositions);
                final long startPosLong = SphereUtils.offsetPos(pos, minDir, lengthMin);
                final int length = lengthMin + lengthMax + 1;
                final long index = SphereUtils.getCompressedPosSide(startPosLong, side);
                int width = length;
                int height = 1;

                // The render method considers width of top and bottom quads as going along the x-axis
                if ((side.getAxis().isVertical() && mainAxis.getAxis() == Direction.Axis.Z) ||
                    mainAxis.getAxis().isVertical())
                {
                    width = 1;
                    height = length;
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
