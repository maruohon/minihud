package fi.dy.masa.minihud.renderer.shapes;

import java.util.HashSet;
import java.util.List;
import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Matrix4f;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.util.BlockSnap;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.util.ShapeRenderType;

public abstract class ShapeCircleBase extends ShapeBase
{
    protected static final Direction[] FACING_ALL = new Direction[] { Direction.DOWN, Direction.UP, Direction.NORTH, Direction.SOUTH, Direction.WEST, Direction.EAST };

    protected BlockSnap snap = BlockSnap.CENTER;
    protected Direction mainAxis = Direction.UP;
    protected double radius;
    protected double radiusSq;
    protected double maxRadius = 256.0; // TODO use per-chunk VBOs or something to allow bigger shapes?
    protected Vec3d center = Vec3d.ZERO;
    protected Vec3d effectiveCenter = Vec3d.ZERO;
    protected Vec3d lastUpdatePos = Vec3d.ZERO;
    protected long lastUpdateTime;

    public ShapeCircleBase(ShapeType type, Color4f color, double radius)
    {
        super(type, color);

        this.setRadius(radius);

        Entity entity = EntityUtils.getCameraEntity();

        if (entity != null)
        {
            Vec3d center = entity.getPos();
            center = new Vec3d(Math.floor(center.x) + 0.5, Math.floor(center.y), Math.floor(center.z) + 0.5);
            this.setCenter(center);
        }
        else
        {
            this.setCenter(Vec3d.ZERO);
        }
    }

    public Vec3d getCenter()
    {
        return this.center;
    }

    public void setCenter(Vec3d center)
    {
        this.center = center;
        this.updateEffectiveCenter();
    }

    public double getRadius()
    {
        return this.radius;
    }

    public void setRadius(double radius)
    {
        if (radius >= 0.0 && radius <= this.maxRadius)
        {
            this.radius = radius;
            this.radiusSq = radius * radius;
            this.setNeedsUpdate();
        }
    }

    public Direction getMainAxis()
    {
        return this.mainAxis;
    }

    public void setMainAxis(Direction mainAxis)
    {
        this.mainAxis = mainAxis;
        this.setNeedsUpdate();
    }

    protected BlockPos getCenterBlock()
    {
        return new BlockPos(this.center);
    }

    public BlockSnap getBlockSnap()
    {
        return this.snap;
    }

    public void setBlockSnap(BlockSnap snap)
    {
        this.snap = snap;
        this.updateEffectiveCenter();
    }

    protected void updateEffectiveCenter()
    {
        Vec3d center = this.center;

        if (this.snap == BlockSnap.CENTER)
        {
            this.effectiveCenter = new Vec3d(Math.floor(center.x) + 0.5, Math.floor(center.y), Math.floor(center.z) + 0.5);
        }
        else if (this.snap == BlockSnap.CORNER)
        {
            this.effectiveCenter = new Vec3d(Math.floor(center.x), Math.floor(center.y), Math.floor(center.z));
        }
        else
        {
            this.effectiveCenter = center;
        }

        this.center = this.effectiveCenter;

        this.setNeedsUpdate();
    }

    protected void onPostUpdate(Vec3d updatePosition)
    {
        this.needsUpdate = false;
        this.lastUpdatePos = updatePosition;
        this.lastUpdateTime = System.currentTimeMillis();
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(VertexFormat.DrawMode.QUADS);
    }

    @Override
    public void draw(MatrixStack matrixStack, Matrix4f projMatrix)
    {
        this.preRender();

        this.renderObjects.get(0).draw(matrixStack, projMatrix);

        // Render the lines as quads with glPolygonMode(GL_LINE)
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE);
        RenderSystem.disableBlend();
        this.renderObjects.get(0).draw(matrixStack, projMatrix);
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL);
        RenderSystem.enableBlend();
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();

        obj.add("center", JsonUtils.vec3dToJson(this.center));
        obj.add("main_axis", new JsonPrimitive(this.mainAxis.name()));
        obj.add("snap", new JsonPrimitive(this.snap.getStringValue()));
        obj.add("radius", new JsonPrimitive(this.radius));

        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        // The snap value has to be set before the center
        if (JsonUtils.hasString(obj, "snap"))
        {
            this.snap = BlockSnap.fromStringStatic(JsonUtils.getString(obj, "snap"));
        }

        if (JsonUtils.hasString(obj, "main_axis"))
        {
            Direction facing = Direction.valueOf(obj.get("main_axis").getAsString());

            if (facing != null)
            {
                this.setMainAxis(facing);
            }
        }

        if (JsonUtils.hasDouble(obj, "radius"))
        {
            this.setRadius(JsonUtils.getDouble(obj, "radius"));
        }

        Vec3d center = JsonUtils.vec3dFromJson(obj, "center");

        if (center != null)
        {
            this.setCenter(center);
        }
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();
        Vec3d c = this.center;

        String aq = GuiBase.TXT_AQUA;
        String bl = GuiBase.TXT_BLUE;
        String gl = GuiBase.TXT_GOLD;
        String gr = GuiBase.TXT_GRAY;
        String rst = GuiBase.TXT_GRAY;

        lines.add(gr + StringUtils.translate("minihud.gui.label.radius_value", gl + String.valueOf(this.getRadius()) + rst));
        lines.add(gr + StringUtils.translate("minihud.gui.label.center_value",
                String.format("x: %s%.2f%s, y: %s%.2f%s, z: %s%.2f%s",
                        bl, c.x, rst, bl, c.y, rst, bl, c.z, rst)));
        lines.add(gr + StringUtils.translate("minihud.gui.label.block_snap", aq + this.snap.getDisplayName() + rst));

        if (this.snap != BlockSnap.NONE)
        {
            c = this.effectiveCenter;
            lines.add(gr + StringUtils.translate("minihud.gui.label.effective_center_value",
                    String.format("x: %s%.2f%s, y: %s%.2f%s, z: %s%.2f%s",
                        bl, c.x, rst, bl, c.y, rst, bl, c.z, rst)));
        }

        return lines;
    }

    protected void renderPositions(HashSet<BlockPos> positions, Direction[] sides, Direction mainAxis, Color4f color, Vec3d cameraPos)
    {
        boolean full = this.renderType == ShapeRenderType.FULL_BLOCK;
        boolean outer = this.renderType == ShapeRenderType.OUTER_EDGE;
        boolean inner = this.renderType == ShapeRenderType.INNER_EDGE;
        LayerRange range = this.layerRange;
        BlockPos.Mutable posMutable = new BlockPos.Mutable();

        for (BlockPos pos : positions)
        {
            if (range.isPositionWithinRange(pos))
            {
                for (int i = 0; i < sides.length; ++i)
                {
                    Direction side = sides[i];
                    posMutable.set(pos.getX() + side.getOffsetX(), pos.getY() + side.getOffsetY(), pos.getZ() + side.getOffsetZ());

                    if (positions.contains(posMutable) == false)
                    {
                        boolean render = full;

                        if (full == false)
                        {
                            boolean onOrIn = this.isPositionOnOrInsideRing(posMutable.getX(), posMutable.getY(), posMutable.getZ(), side, mainAxis);
                            render |= ((outer && onOrIn == false) || (inner && onOrIn));
                        }

                        if (render)
                        {
                            drawBlockSpaceSideBatchedQuads(pos, side, color, 0, cameraPos, BUFFER_1);
                        }
                    }
                }
            }
        }
    }

    protected void addPositionsOnHorizontalRing(HashSet<BlockPos> positions, BlockPos.Mutable posMutable, Direction direction)
    {
        if (this.movePositionToRing(posMutable, direction, Direction.UP))
        {
            BlockPos posFirst = posMutable.toImmutable();
            positions.add(posFirst);
            double r = (double) this.radius;
            int failsafe = (int) (2.5 * Math.PI * r); // somewhat over double the circumference

            while (--failsafe > 0)
            {
                direction = this.getNextPositionOnHorizontalRing(posMutable, direction);

                if (direction == null || posMutable.equals(posFirst))
                {
                    break;
                }

                positions.add(posMutable.toImmutable());
            }
        }
    }

    protected void addPositionsOnVerticalRing(HashSet<BlockPos> positions, BlockPos.Mutable posMutable, Direction direction, Direction mainAxis)
    {
        if (this.movePositionToRing(posMutable, direction, mainAxis))
        {
            BlockPos posFirst = posMutable.toImmutable();
            positions.add(posFirst);
            double r = (double) this.radius;
            int failsafe = (int) (2.5 * Math.PI * r); // somewhat over double the circumference

            while (--failsafe > 0)
            {
                direction = this.getNextPositionOnVerticalRing(posMutable, direction, mainAxis);

                if (direction == null || posMutable.equals(posFirst))
                {
                    break;
                }

                positions.add(posMutable.toImmutable());
            }
        }
    }

    protected boolean movePositionToRing(BlockPos.Mutable posMutable, Direction dir, Direction mainAxis)
    {
        int x = posMutable.getX();
        int y = posMutable.getY();
        int z = posMutable.getZ();
        int xNext = x;
        int yNext = y;
        int zNext = z;
        int failsafe = 0;
        final int failsafeMax = (int) this.radius + 5;

        while (this.isPositionOnOrInsideRing(xNext, yNext, zNext, dir, mainAxis) && ++failsafe < failsafeMax)
        {
            x = xNext;
            y = yNext;
            z = zNext;
            xNext += dir.getOffsetX();
            yNext += dir.getOffsetY();
            zNext += dir.getOffsetZ();
        }

        // Successfully entered the loop at least once
        if (failsafe > 0)
        {
            posMutable.set(x, y, z);
            return true;
        }

        return false;
    }

    @Nullable
    protected Direction getNextPositionOnHorizontalRing(BlockPos.Mutable posMutable, Direction dir)
    {
        Direction dirOut = dir;
        Direction ccw90 = getNextDirRotating(dir);
        final int y = posMutable.getY();

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getOffsetX();
            int z = posMutable.getZ() + dir.getOffsetZ();

            // First check the adjacent position
            if (this.isPositionOnOrInsideRing(x, y, z, dir, Direction.UP))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x += ccw90.getOffsetX();
            z += ccw90.getOffsetZ();

            if (this.isPositionOnOrInsideRing(x, y, z, dir, Direction.UP))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Delay the next direction by one cycle, so that it won't get updated too soon on the diagonals
            dirOut = dir;
            dir = getNextDirRotating(dir);
            ccw90 = getNextDirRotating(dir);
        }

        return null;
    }

    @Nullable
    protected Direction getNextPositionOnVerticalRing(BlockPos.Mutable posMutable, Direction dir, Direction mainAxis)
    {
        Direction dirOut = dir;
        Direction ccw90 = getNextDirRotatingVertical(dir, mainAxis);

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getOffsetX();
            int y = posMutable.getY() + dir.getOffsetY();
            int z = posMutable.getZ() + dir.getOffsetZ();

            // First check the adjacent position
            if (this.isPositionOnOrInsideRing(x, y, z, dir, mainAxis))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x += ccw90.getOffsetX();
            y += ccw90.getOffsetY();
            z += ccw90.getOffsetZ();

            if (this.isPositionOnOrInsideRing(x, y, z, dir, mainAxis))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Delay the next direction by one cycle, so that it won't get updated too soon on the diagonals
            dirOut = dir;
            dir = getNextDirRotatingVertical(dir, mainAxis);
            ccw90 = getNextDirRotatingVertical(dir, mainAxis);
        }

        return null;
    }

    protected boolean isPositionOnOrInsideRing(int blockX, int blockY, int blockZ, Direction outSide, Direction mainAxis)
    {
        double x = (double) blockX + 0.5;
        double y = (double) blockY + 0.5;
        double z = (double) blockZ + 0.5;
        double dist = this.effectiveCenter.squaredDistanceTo(x, y, z);
        double diff = this.radiusSq - dist;

        if (diff > 0)
        {
            return true;
        }

        double xAdj = (double) blockX + outSide.getOffsetX() + 0.5;
        double yAdj = (double) blockY + outSide.getOffsetY() + 0.5;
        double zAdj = (double) blockZ + outSide.getOffsetZ() + 0.5;
        double distAdj = this.effectiveCenter.squaredDistanceTo(xAdj, yAdj, zAdj);
        double diffAdj = this.radiusSq - distAdj;

        return diffAdj > 0 && Math.abs(diff) < Math.abs(diffAdj);
    }

    protected boolean isAdjacentPositionOutside(BlockPos pos, Direction dir, Direction mainAxis)
    {
        return this.isPositionOnOrInsideRing(pos.getX() + dir.getOffsetX(), pos.getY() + dir.getOffsetY(), pos.getZ() + dir.getOffsetZ(), dir, mainAxis) == false;
    }

    /**
     * Returns the next horizontal direction in sequence, rotating counter-clockwise
     * @param dirIn
     * @return
     */
    protected static Direction getNextDirRotating(Direction dirIn)
    {
        switch (dirIn)
        {
            case EAST:  return Direction.NORTH;
            case NORTH: return Direction.WEST;
            case WEST:  return Direction.SOUTH;
            case SOUTH: return Direction.EAST;
            default:    return Direction.NORTH;
        }
    }

    /**
     * Returns the next direction in sequence, rotating up to north
     * @param dirIn
     * @return
     */
    protected static Direction getNextDirRotatingVertical(Direction dirIn, Direction mainAxis)
    {
        switch (mainAxis)
        {
            case UP:
            case DOWN:
                switch (dirIn)
                {
                    case UP:    return Direction.NORTH;
                    case NORTH: return Direction.DOWN;
                    case DOWN:  return Direction.SOUTH;
                    case SOUTH: return Direction.UP;
                    default:    return Direction.NORTH;
                }

            case NORTH:
            case SOUTH:
                switch (dirIn)
                {
                    case UP:    return Direction.EAST;
                    case EAST:  return Direction.DOWN;
                    case DOWN:  return Direction.WEST;
                    case WEST:  return Direction.UP;
                    default:    return Direction.EAST;
                }

            case WEST:
            case EAST:
                switch (dirIn)
                {
                    case UP:    return Direction.SOUTH;
                    case SOUTH: return Direction.DOWN;
                    case DOWN:  return Direction.NORTH;
                    case NORTH: return Direction.UP;
                    default:    return Direction.SOUTH;
                }
        }

        return Direction.UP;
    }

    /**
     * Assumes a BufferBuilder in GL_QUADS mode has been initialized
     */
    public static void drawBlockSpaceSideBatchedQuads(BlockPos pos, Direction side, Color4f color, double expand, Vec3d cameraPos, BufferBuilder buffer)
    {
        double minX = pos.getX() - expand - cameraPos.x;
        double minY = pos.getY() - expand - cameraPos.y;
        double minZ = pos.getZ() - expand - cameraPos.z;
        double maxX = pos.getX() + expand + 1 - cameraPos.x;
        double maxY = pos.getY() + expand + 1 - cameraPos.y;
        double maxZ = pos.getZ() + expand + 1 - cameraPos.z;

        switch (side)
        {
            case DOWN:
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case UP:
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case NORTH:
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case SOUTH:
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case WEST:
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case EAST:
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                break;
        }
    }
}
