package fi.dy.masa.minihud.renderer.shapes;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.config.values.BlockSnap;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager.ShapeTypes;

public class ShapeSphereBlocky extends ShapeBase
{
    protected static final EnumFacing[] FACING_ALL = new EnumFacing[] { EnumFacing.DOWN, EnumFacing.UP, EnumFacing.NORTH, EnumFacing.SOUTH, EnumFacing.WEST, EnumFacing.EAST };

    protected double radius;
    protected double radiusSq;
    protected BlockSnap snap = BlockSnap.CENTER;
    protected Vec3d center = Vec3d.ZERO;
    protected Vec3d effectiveCenter = Vec3d.ZERO;
    protected Vec3d lastUpdatePos = Vec3d.ZERO;
    protected long lastUpdateTime;

    public ShapeSphereBlocky()
    {
        this(ShapeTypes.SPHERE_BLOCKY, Configs.Colors.SHAPE_SPHERE_BLOCKY.getColor(), 16);
    }

    public ShapeSphereBlocky(ShapeTypes type, Color4f color, double radius)
    {
        super(type, color);

        this.setRadius(radius);

        Entity entity = EntityUtils.getCameraEntity();

        if (entity != null)
        {
            Vec3d center = entity.getPositionVector();
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
        if (radius >= 0.0 && radius <= 256) // TODO use per-chunk VBOs or something to allow bigger shapes?
        {
            this.radius = radius;
            this.radiusSq = radius * radius;
            this.setNeedsUpdate();
        }
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

        this.setNeedsUpdate();
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        this.renderSphereShape();

        this.needsUpdate = false;
        this.lastUpdatePos = entity.getPositionVector();
        this.lastUpdateTime = System.currentTimeMillis();
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
    }

    @Override
    public void draw(double x, double y, double z)
    {
        GlStateManager.pushMatrix();
        this.preRender(x, y, z);

        this.renderObjects.get(0).draw();

        // Render the lines as quads with glPolygonMode(GL_LINE)
        GlStateManager.glPolygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE);
        GlStateManager.disableBlend();
        this.renderObjects.get(0).draw();
        GlStateManager.glPolygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL);
        GlStateManager.enableBlend();

        GlStateManager.popMatrix();
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();

        obj.add("center", JsonUtils.vec3dToJson(this.center));
        obj.add("snap", new JsonPrimitive(this.snap.getStringValue()));
        obj.add("color", new JsonPrimitive(this.color.intValue));

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

        lines.add(StringUtils.translate("minihud.gui.label.block_snap", this.snap.getDisplayName()));
        lines.add(StringUtils.translate("minihud.gui.label.center_value", String.format("x: %.2f, y: %.2f, z: %.2f", c.x, c.y, c.z)));

        if (this.snap != BlockSnap.NONE)
        {
            c = this.effectiveCenter;
            lines.add(StringUtils.translate("minihud.gui.label.effective_center_value", String.format("x: %.2f, y: %.2f, z: %.2f", c.x, c.y, c.z)));
        }

        return lines;
    }

    protected void renderSphereShape()
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        Color4f colorQuad = this.color;
        BlockPos posCenter = this.getCenterBlock();
        BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();
        HashSet<BlockPos> spherePositions = new HashSet<>();

        this.setPosition(posCenter);

        //long before = System.nanoTime();
        posMutable.setPos(posCenter);
        this.addPositionsOnRing(spherePositions, posMutable, EnumFacing.EAST);

        posMutable.setPos(posCenter);
        this.addPositionsOnRing(spherePositions, posMutable, EnumFacing.UP);

        final int r = (int) this.radius + 2;

        for (int i = 1; i < r; ++i)
        {
            // Horizontal rings
            posMutable.setPos(posCenter.getX(), posCenter.getY() - i, posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, EnumFacing.EAST);

            posMutable.setPos(posCenter.getX(), posCenter.getY() + i, posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, EnumFacing.EAST);

            // Vertical rings
            posMutable.setPos(posCenter.getX() - i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, EnumFacing.UP);

            posMutable.setPos(posCenter.getX() + i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnRing(spherePositions, posMutable, EnumFacing.UP);
        }
        //System.out.printf("time: %.6f s - margin: %.4f\n", (double) (System.nanoTime() - before) / 1000000000D, this.margin);
        //System.out.printf("spherePositions: %d\n", spherePositions.size());

        for (BlockPos pos : spherePositions)
        {
            for (int i = 0; i < 6; ++i)
            {
                EnumFacing side = FACING_ALL[i];
                posMutable.setPos(pos).move(side);

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

        BUFFER_1.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
    }

    protected void addPositionsOnRing(HashSet<BlockPos> positions, BlockPos.MutableBlockPos posMutable, EnumFacing direction)
    {
        if (this.movePositionToRing(posMutable, direction))
        {
            BlockPos posFirst = posMutable.toImmutable();
            positions.add(posFirst);
            double r = (double) this.radius;
            int failsafe = (int) (2.5 * Math.PI * r); // somewhat over double the circumference
            EnumFacing.Axis axis = direction.getAxis();

            while (--failsafe > 0)
            {
                if (axis == EnumFacing.Axis.Y)
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

    protected boolean movePositionToRing(BlockPos.MutableBlockPos posMutable, EnumFacing dir)
    {
        int x = posMutable.getX();
        int y = posMutable.getY();
        int z = posMutable.getZ();
        int xNext = x;
        int yNext = y;
        int zNext = z;
        int failsafe = 0;
        final int failsafeMax = (int) this.radius + 5;

        while (this.isPositionOnOrInsideSphere(xNext, yNext, zNext, dir) && ++failsafe < failsafeMax)
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
            posMutable.setPos(x, y, z);
            return true;
        }

        return false;
    }

    @Nullable
    protected EnumFacing getNextPositionOnRing(BlockPos.MutableBlockPos posMutable, EnumFacing dir)
    {
        EnumFacing dirOut = dir;
        EnumFacing ccw90 = getNextDirRotating(dir);
        final int y = posMutable.getY();

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getXOffset();
            int z = posMutable.getZ() + dir.getZOffset();

            // First check the adjacent position
            if (this.isPositionOnOrInsideSphere(x, y, z, dir))
            {
                posMutable.setPos(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x += ccw90.getXOffset();
            z += ccw90.getZOffset();

            if (this.isPositionOnOrInsideSphere(x, y, z, dir))
            {
                posMutable.setPos(x, y, z);
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
    protected EnumFacing getNextPositionOnRingVertical(BlockPos.MutableBlockPos posMutable, EnumFacing dir)
    {
        EnumFacing dirOut = dir;
        EnumFacing ccw90 = getNextDirRotatingVertical(dir);

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getXOffset();
            int y = posMutable.getY() + dir.getYOffset();
            int z = posMutable.getZ() + dir.getZOffset();

            // First check the adjacent position
            if (this.isPositionOnOrInsideSphere(x, y, z, dir))
            {
                posMutable.setPos(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x += ccw90.getXOffset();
            y += ccw90.getYOffset();
            z += ccw90.getZOffset();

            if (this.isPositionOnOrInsideSphere(x, y, z, dir))
            {
                posMutable.setPos(x, y, z);
                return dirOut;
            }

            // Delay the next direction by one cycle, so that it won't get updated too soon on the diagonals
            dirOut = dir;
            dir = getNextDirRotatingVertical(dir);
            ccw90 = getNextDirRotatingVertical(dir);
        }

        return null;
    }

    protected boolean isPositionOnOrInsideSphere(int blockX, int blockY, int blockZ, EnumFacing outSide)
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

    protected boolean isAdjacentPositionOutside(BlockPos pos, EnumFacing dir)
    {
        return this.isPositionOnOrInsideSphere(pos.getX() + dir.getXOffset(), pos.getY() + dir.getYOffset(), pos.getZ() + dir.getZOffset(), dir) == false;
    }

    /**
     * Returns the next horizontal direction in sequence, rotating counter-clockwise
     * @param dirIn
     * @return
     */
    protected static EnumFacing getNextDirRotating(EnumFacing dirIn)
    {
        switch (dirIn)
        {
            case EAST:  return EnumFacing.NORTH;
            case NORTH: return EnumFacing.WEST;
            case WEST:  return EnumFacing.SOUTH;
            case SOUTH: return EnumFacing.EAST;
            default:    return EnumFacing.NORTH;
        }
    }

    /**
     * Returns the next direction in sequence, rotating up to north
     * @param dirIn
     * @return
     */
    protected static EnumFacing getNextDirRotatingVertical(EnumFacing dirIn)
    {
        switch (dirIn)
        {
            case UP:    return EnumFacing.NORTH;
            case NORTH: return EnumFacing.DOWN;
            case DOWN:  return EnumFacing.SOUTH;
            case SOUTH: return EnumFacing.UP;
            default:    return EnumFacing.NORTH;
        }
    }

    public static void renderBlockSideQuads(BlockPos pos, EnumFacing side, BufferBuilder buffer, Color4f color)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        switch (side)
        {
            case DOWN:
                buffer.pos(x    , y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case UP:
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case NORTH:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case SOUTH:
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case WEST:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case EAST:
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
        }
    }

    public static void renderBlockSideLines(BlockPos pos, EnumFacing side, BufferBuilder buffer, Color4f color)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        switch (side)
        {
            case DOWN:
                buffer.pos(x    , y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case UP:
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case NORTH:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case SOUTH:
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case WEST:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case EAST:
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
        }
    }
}
