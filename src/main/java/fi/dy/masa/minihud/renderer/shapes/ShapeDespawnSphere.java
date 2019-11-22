package fi.dy.masa.minihud.renderer.shapes;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mojang.blaze3d.platform.GlStateManager;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.BlockSnap;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.Quadrant;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager.ShapeTypes;

public class ShapeDespawnSphere extends ShapeBase
{
    private static final Direction[] FACING_ALL = new Direction[] { Direction.DOWN, Direction.UP, Direction.NORTH, Direction.SOUTH, Direction.WEST, Direction.EAST };

    protected Vec3d center = Vec3d.ZERO;
    protected Vec3d effectiveCenter = Vec3d.ZERO;
    protected final Vec3d[] quadrantCenters = new Vec3d[4];
    protected Vec3d lastUpdatePos = Vec3d.ZERO;
    protected BlockSnap snap = BlockSnap.NONE;
    protected double margin = 0.5;
    protected long lastUpdateTime;

    public ShapeDespawnSphere()
    {
        super(ShapeTypes.DESPAWN_SPHERE, Configs.Colors.DESPAWN_SPHERE_OVERLAY_COLOR.getColor());

        if (this.mc.player != null)
        {
            this.setCenter(this.mc.player.getPos());
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

    private void updateQuadrantPoints()
    {
        Vec3d center = this.effectiveCenter;

        this.quadrantCenters[Quadrant.NORTH_WEST.ordinal()] = new Vec3d(center.x - this.margin, center.y, center.z - this.margin);
        this.quadrantCenters[Quadrant.NORTH_EAST.ordinal()] = new Vec3d(center.x + this.margin, center.y, center.z - this.margin);
        this.quadrantCenters[Quadrant.SOUTH_WEST.ordinal()] = new Vec3d(center.x - this.margin, center.y, center.z + this.margin);
        this.quadrantCenters[Quadrant.SOUTH_EAST.ordinal()] = new Vec3d(center.x + this.margin, center.y, center.z + this.margin);

        this.setNeedsUpdate();
    }

    private void updateEffectiveCenter()
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

        this.updateQuadrantPoints();
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

    public BlockSnap getBlockSnap()
    {
        return this.snap;
    }

    public void setBlockSnap(BlockSnap snap)
    {
        this.snap = snap;
        this.updateEffectiveCenter();
    }

    @Override
    public void update(Entity entity, MinecraftClient mc)
    {
        this.renderSphereBlock();

        this.needsUpdate = false;
        this.lastUpdatePos = entity.getPos();
        this.lastUpdateTime = System.currentTimeMillis();
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
    }

    @Override
    public void draw(double x, double y, double z, net.minecraft.client.util.math.MatrixStack matrixStack)
    {
        GlStateManager.pushMatrix();
        this.preRender(x, y, z);

        matrixStack.push();
        matrixStack.translate(-x, -y, -z);

        this.renderObjects.get(0).draw(matrixStack);

        // Render the lines as quads with glPolygonMode(GL_LINE)
        GlStateManager.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE);
        GlStateManager.disableBlend();
        this.renderObjects.get(0).draw(matrixStack);
        GlStateManager.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL);
        GlStateManager.enableBlend();

        matrixStack.pop();

        GlStateManager.popMatrix();
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();

        obj.add("center", JsonUtils.vec3dToJson(this.center));
        obj.add("snap", new JsonPrimitive(this.snap.getStringValue()));
        obj.add("margin", new JsonPrimitive(this.margin));
        obj.add("color", new JsonPrimitive(this.color.intValue));

        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        this.margin = JsonUtils.getDouble(obj, "margin");

        // The snap value has to be set before the center
        if (JsonUtils.hasString(obj, "snap"))
        {
            this.snap = BlockSnap.fromStringStatic(JsonUtils.getString(obj, "snap"));
        }

        Vec3d center = JsonUtils.vec3dFromJson(obj, "center");

        if (center != null)
        {
            this.setCenter(center);
        }

        if (JsonUtils.hasInteger(obj, "color"))
        {
            this.color = Color4f.fromColor(JsonUtils.getInteger(obj, "color"));
        }
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = new ArrayList<>();
        Vec3d c = this.center;
        lines.add(StringUtils.translate("minihud.gui.label.center_value", String.format("x: %.2f, y: %.2f, z: %.2f", c.x, c.y, c.z)));
        lines.add(StringUtils.translate("minihud.gui.label.block_snap", this.snap.getDisplayName()));
        lines.add(StringUtils.translate("minihud.gui.label.margin_value", String.format("%.2f", this.margin)));

        if (this.snap != BlockSnap.NONE)
        {
            c = this.effectiveCenter;
            lines.add(StringUtils.translate("minihud.gui.label.effective_center_value", String.format("x: %.2f, y: %.2f, z: %.2f", c.x, c.y, c.z)));
        }

        return lines;
    }

    protected void renderSphereBlock()
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        Color4f colorQuad = this.color;
        BlockPos posCenter = new BlockPos(this.effectiveCenter);
        BlockPos.Mutable posMutable = new BlockPos.Mutable();
        HashSet<BlockPos> spherePositions = new HashSet<>();

        //long before = System.nanoTime();
        posMutable.set(posCenter);
        this.addPositionsOnRing(spherePositions, posMutable, Direction.EAST);

        posMutable.set(posCenter);
        this.addPositionsOnRing(spherePositions, posMutable, Direction.UP);

        for (int i = 1; i < 130; ++i)
        {
            // Horizontal rings
            posMutable.set(posCenter.getX(), posCenter.getY() - i, posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, Direction.EAST);

            posMutable.set(posCenter.getX(), posCenter.getY() + i, posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, Direction.EAST);

            // Vertical rings
            posMutable.set(posCenter.getX() - i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, Direction.UP);

            posMutable.set(posCenter.getX() + i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, Direction.UP);
        }
        //System.out.printf("time: %.6f s - margin: %.4f\n", (double) (System.nanoTime() - before) / 1000000000D, this.margin);

        for (BlockPos pos : spherePositions)
        {
            for (int i = 0; i < 6; ++i)
            {
                Direction side = FACING_ALL[i];
                posMutable.set(pos).setOffset(side);

                if (this.layerRange.isPositionWithinRange(pos) &&
                    spherePositions.contains(posMutable) == false &&
                    this.isAdjacentPositionOutside(pos, side))
                {
                    renderBlockSideQuads(pos, side, BUFFER_1, colorQuad);
                    //renderBlockSideLines(pos, side, BUFFER_2, colorLine);
                    //r++;
                }
            }
        }
        //System.out.printf("rendered: %d\n", r);

        BUFFER_1.end();

        renderQuads.uploadData(BUFFER_1);
    }

    private void addPositionsOnRing(HashSet<BlockPos> positions, BlockPos.Mutable posMutable, Direction direction)
    {
        if (this.movePositionToRing(posMutable, direction))
        {
            BlockPos posFirst = posMutable.toImmutable();
            positions.add(posFirst);
            int failsafe = 860;
            Direction.Axis axis = direction.getAxis();

            while (--failsafe > 0)
            {
                if (axis == Direction.Axis.Y)
                {
                    direction = this.getNextPositionOnRingVertical(posMutable, direction);
                }
                else
                {
                    direction = this.getNextPositionOnRing(posMutable, direction);
                }

                if (direction == null || posMutable.equals(posFirst))
                {
                    break;
                }

                positions.add(posMutable.toImmutable());
            }
        }
    }

    private boolean movePositionToRing(BlockPos.Mutable posMutable, Direction dir)
    {
        int x = posMutable.getX();
        int y = posMutable.getY();
        int z = posMutable.getZ();
        int xNext = x;
        int yNext = y;
        int zNext = z;
        int failsafe = 0;
        final int failsafeMax = 140;

        while (this.isPositionWithinRange(xNext, yNext, zNext) && ++failsafe < failsafeMax)
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
    private Direction getNextPositionOnRing(BlockPos.Mutable posMutable, Direction dir)
    {
        Direction dirOut = dir;
        Direction ccw90 = getNextDirRotating(dir);
        final int y = posMutable.getY();

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getOffsetX();
            int z = posMutable.getZ() + dir.getOffsetZ();

            // First check the adjacent position
            if (this.isPositionWithinRange(x, y, z))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x = posMutable.getX() + dir.getOffsetX() + ccw90.getOffsetX();
            z = posMutable.getZ() + dir.getOffsetZ() + ccw90.getOffsetZ();

            if (this.isPositionWithinRange(x, y, z))
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
    private Direction getNextPositionOnRingVertical(BlockPos.Mutable posMutable, Direction dir)
    {
        Direction dirOut = dir;
        Direction ccw90 = getNextDirRotatingVertical(dir);

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getOffsetX();
            int y = posMutable.getY() + dir.getOffsetY();
            int z = posMutable.getZ() + dir.getOffsetZ();

            // First check the adjacent position
            if (this.isPositionWithinRange(x, y, z))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x = posMutable.getX() + dir.getOffsetX() + ccw90.getOffsetX();
            y = posMutable.getY() + dir.getOffsetY() + ccw90.getOffsetY();
            z = posMutable.getZ() + dir.getOffsetZ() + ccw90.getOffsetZ();

            if (this.isPositionWithinRange(x, y, z))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Delay the next direction by one cycle, so that it won't get updated too soon on the diagonals
            dirOut = dir;
            dir = getNextDirRotatingVertical(dir);
            ccw90 = getNextDirRotatingVertical(dir);
        }

        return null;
    }

    private boolean isPositionWithinRange(int x, int y, int z)
    {
        final double maxDistSq = 128 * 128;
        Vec3d quadrantCenter = this.quadrantCenters[Quadrant.getQuadrant(x, z, this.effectiveCenter).ordinal()];
        double dx = x + 0.5;
        double dy = y + 1;
        double dz = z + 0.5;

        return quadrantCenter.squaredDistanceTo(dx, dy, dz) < maxDistSq || this.effectiveCenter.squaredDistanceTo(dx, dy, dz) < maxDistSq;
    }

    private boolean isAdjacentPositionOutside(BlockPos pos, Direction dir)
    {
        final double maxDistSq = 128 * 128;
        Vec3d quadrantCenter = this.quadrantCenters[Quadrant.getQuadrant(pos.getX(), pos.getZ(), this.effectiveCenter).ordinal()];
        double x = pos.getX() + dir.getOffsetX() + 0.5;
        double y = pos.getY() + dir.getOffsetY() + 1;
        double z = pos.getZ() + dir.getOffsetZ() + 0.5;

        return quadrantCenter.squaredDistanceTo(x, y, z) >= maxDistSq && this.effectiveCenter.squaredDistanceTo(x, y, z) >= maxDistSq;
    }

    /**
     * Returns the next horizontal direction in sequence, rotating counter-clockwise
     * @param dirIn
     * @return
     */
    private static Direction getNextDirRotating(Direction dirIn)
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
    private static Direction getNextDirRotatingVertical(Direction dirIn)
    {
        switch (dirIn)
        {
            case UP:    return Direction.NORTH;
            case NORTH: return Direction.DOWN;
            case DOWN:  return Direction.SOUTH;
            case SOUTH: return Direction.UP;
            default:    return Direction.NORTH;
        }
    }

    public static void renderBlockSideQuads(BlockPos pos, Direction side, BufferBuilder buffer, Color4f color)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        switch (side)
        {
            case DOWN:
                buffer.vertex(x    , y, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).color(color.r, color.g, color.b, color.a).next();
                break;
            case UP:
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                break;
            case NORTH:
                buffer.vertex(x    , y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).next();
                break;
            case SOUTH:
                buffer.vertex(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                break;
            case WEST:
                buffer.vertex(x    , y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                break;
            case EAST:
                buffer.vertex(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                break;
        }
    }

    public static void renderBlockSideLines(BlockPos pos, Direction side, BufferBuilder buffer, Color4f color)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        switch (side)
        {
            case DOWN:
                buffer.vertex(x    , y, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z    ).color(color.r, color.g, color.b, color.a).next();
                break;
            case UP:
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                break;
            case NORTH:
                buffer.vertex(x    , y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y    , z    ).color(color.r, color.g, color.b, color.a).next();
                break;
            case SOUTH:
                buffer.vertex(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                break;
            case WEST:
                buffer.vertex(x    , y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y    , z    ).color(color.r, color.g, color.b, color.a).next();
                break;
            case EAST:
                buffer.vertex(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).next();
                break;
        }
    }
}
