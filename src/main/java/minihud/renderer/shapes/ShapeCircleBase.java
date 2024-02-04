package minihud.renderer.shapes;

import java.text.DecimalFormat;
import java.util.HashSet;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.lwjgl.opengl.GL11;

import net.minecraft.entity.Entity;

import malilib.config.value.BaseOptionListConfigValue;
import malilib.config.value.BlockSnap;
import malilib.render.ShapeRenderUtils;
import malilib.util.StringUtils;
import malilib.util.data.Color4f;
import malilib.util.data.json.JsonUtils;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import malilib.util.game.wrap.RenderWrap;
import malilib.util.position.BlockPos;
import malilib.util.position.Direction;
import malilib.util.position.LayerRange;
import malilib.util.position.Vec3d;
import minihud.util.value.ShapeRenderType;

public abstract class ShapeCircleBase extends ShapeBase
{
    protected BlockSnap snap = BlockSnap.CENTER;
    protected Direction mainAxis = Direction.UP;
    protected double radius;
    protected double radiusSq;
    protected double maxRadius = 1024.0; // TODO use per-chunk VBOs or something to allow bigger shapes?
    protected Vec3d center = Vec3d.ZERO;
    protected Vec3d effectiveCenter = Vec3d.ZERO;
    protected Vec3d lastUpdatePos = Vec3d.ZERO;

    public ShapeCircleBase(ShapeType type, Color4f color, double radius)
    {
        super(type, color);

        this.setRadius(radius);

        Entity entity = GameUtils.getCameraEntity();

        if (entity != null)
        {
            Vec3d center = EntityWrap.getEntityPos(entity);
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
        return BlockPos.ofFloored(this.center);
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
    }

    @Override
    public void allocateGlResources()
    {
        this.quadRenderer = this.allocateBuffer(GL11.GL_QUADS);
    }

    @Override
    protected void startBuffers()
    {
        this.quadBuilder.start();
    }

    @Override
    protected void uploadBuffers()
    {
        this.quadRenderer.uploadData(this.quadBuilder);
        this.needsUpdate = false;
    }

    @Override
    public void draw()
    {
        this.preRender();

        this.quadRenderer.draw();

        // Render the lines as quads with glPolygonMode(GL_LINE)
        RenderWrap.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE);
        RenderWrap.disableBlend();
        this.quadRenderer.draw();

        RenderWrap.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL);
        RenderWrap.enableBlend();

        this.postRender();
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();

        obj.add("center", JsonUtils.vec3dToJson(this.center));
        obj.add("main_axis", new JsonPrimitive(this.mainAxis.name()));
        obj.add("snap", new JsonPrimitive(this.snap.getName()));
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
            this.snap = BaseOptionListConfigValue.findValueByName(JsonUtils.getString(obj, "snap"), BlockSnap.VALUES);
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

        Vec3d center = JsonUtils.getVec3d(obj, "center");

        if (center != null)
        {
            this.setCenter(center);
        }
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();
        DecimalFormat fmt = new DecimalFormat("#.##");
        Vec3d c = this.center;

        lines.add(StringUtils.translate("minihud.hover.shape.radius", this.getRadius()));
        lines.add(StringUtils.translate("minihud.hover.shape.center", fmt.format(c.x), fmt.format(c.y), fmt.format(c.z)));
        lines.add(StringUtils.translate("minihud.hover.shape.block_snap", this.snap.getDisplayName()));

        if (this.snap != BlockSnap.NONE)
        {
            c = this.effectiveCenter;
            lines.add(StringUtils.translate("minihud.hover.shape.effective_center", fmt.format(c.x), fmt.format(c.y), fmt.format(c.z)));
        }

        return lines;
    }

    protected void renderPositions(HashSet<BlockPos> positions, Direction[] sides, Direction mainAxis, Color4f color, Vec3d cameraPos)
    {
        boolean full = this.renderType == ShapeRenderType.FULL_BLOCK;
        boolean outer = this.renderType == ShapeRenderType.OUTER_EDGE;
        boolean inner = this.renderType == ShapeRenderType.INNER_EDGE;
        LayerRange range = this.layerRange;
        BlockPos.MutBlockPos posMutable = new BlockPos.MutBlockPos();

        for (BlockPos pos : positions)
        {
            if (range.isPositionWithinRange(pos))
            {
                for (Direction side : sides)
                {
                    posMutable.set(pos.getX() + side.getXOffset(), pos.getY() + side.getYOffset(), pos.getZ() + side.getZOffset());

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
                            ShapeRenderUtils.renderBlockPosSideQuad(pos, side, 0, color, cameraPos, this.quadBuilder);
                        }
                    }
                }
            }
        }
    }

    protected void addPositionsOnHorizontalRing(HashSet<BlockPos> positions, BlockPos.MutBlockPos posMutable, Direction direction)
    {
        if (this.movePositionToRing(posMutable, direction, Direction.UP))
        {
            BlockPos posFirst = posMutable.toImmutable();
            positions.add(posFirst);
            double r = this.radius;
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

    protected void addPositionsOnVerticalRing(HashSet<BlockPos> positions, BlockPos.MutBlockPos posMutable, Direction direction, Direction mainAxis)
    {
        if (this.movePositionToRing(posMutable, direction, mainAxis))
        {
            BlockPos posFirst = posMutable.toImmutable();
            positions.add(posFirst);
            double r = this.radius;
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

    protected boolean movePositionToRing(BlockPos.MutBlockPos posMutable, Direction dir, Direction mainAxis)
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
            xNext += dir.getXOffset();
            yNext += dir.getYOffset();
            zNext += dir.getZOffset();
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
    protected Direction getNextPositionOnHorizontalRing(BlockPos.MutBlockPos posMutable, Direction dir)
    {
        Direction dirOut = dir;
        Direction ccw90 = getNextDirRotating(dir);
        final int y = posMutable.getY();

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getXOffset();
            int z = posMutable.getZ() + dir.getZOffset();

            // First check the adjacent position
            if (this.isPositionOnOrInsideRing(x, y, z, dir, Direction.UP))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x += ccw90.getXOffset();
            z += ccw90.getZOffset();

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
    protected Direction getNextPositionOnVerticalRing(BlockPos.MutBlockPos posMutable, Direction dir, Direction mainAxis)
    {
        Direction dirOut = dir;
        Direction ccw90 = getNextDirRotatingVertical(dir, mainAxis);

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getXOffset();
            int y = posMutable.getY() + dir.getYOffset();
            int z = posMutable.getZ() + dir.getZOffset();

            // First check the adjacent position
            if (this.isPositionOnOrInsideRing(x, y, z, dir, mainAxis))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x += ccw90.getXOffset();
            y += ccw90.getYOffset();
            z += ccw90.getZOffset();

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
        double dist = this.effectiveCenter.squareDistanceTo(x, y, z);
        double diff = this.radiusSq - dist;

        if (diff > 0)
        {
            return true;
        }

        double xAdj = (double) blockX + outSide.getXOffset() + 0.5;
        double yAdj = (double) blockY + outSide.getYOffset() + 0.5;
        double zAdj = (double) blockZ + outSide.getZOffset() + 0.5;
        double distAdj = this.effectiveCenter.squareDistanceTo(xAdj, yAdj, zAdj);
        double diffAdj = this.radiusSq - distAdj;

        return diffAdj > 0 && Math.abs(diff) < Math.abs(diffAdj);
    }

    protected boolean isAdjacentPositionOutside(BlockPos pos, Direction dir, Direction mainAxis)
    {
        return this.isPositionOnOrInsideRing(pos.getX() + dir.getXOffset(), pos.getY() + dir.getYOffset(), pos.getZ() + dir.getZOffset(), dir, mainAxis) == false;
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
            case NORTH: return Direction.WEST;
            case WEST:  return Direction.SOUTH;
            case SOUTH: return Direction.EAST;
            case EAST:
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
                    case NORTH: return Direction.DOWN;
                    case DOWN:  return Direction.SOUTH;
                    case SOUTH: return Direction.UP;
                    case UP:
                    default:    return Direction.NORTH;
                }

            case NORTH:
            case SOUTH:
                switch (dirIn)
                {
                    case EAST:  return Direction.DOWN;
                    case DOWN:  return Direction.WEST;
                    case WEST:  return Direction.UP;
                    case UP:
                    default:    return Direction.EAST;
                }

            case WEST:
            case EAST:
                switch (dirIn)
                {
                    case SOUTH: return Direction.DOWN;
                    case DOWN:  return Direction.NORTH;
                    case NORTH: return Direction.UP;
                    case UP:
                    default:    return Direction.SOUTH;
                }
        }

        return Direction.UP;
    }
}
