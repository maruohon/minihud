package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
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
import fi.dy.masa.minihud.util.shape.SphereUtils;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public abstract class ShapeCircleBase extends ShapeBase
{
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

        lines.add(gr + StringUtils.translate("minihud.gui.label.radius_value", gl + this.getRadius() + rst));
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

    protected void renderPositions(LongOpenHashSet positions,
                                   Direction[] sides,
                                   SphereUtils.RingPositionTest test,
                                   Color4f color,
                                   double expand,
                                   Vec3d cameraPos)
    {
        boolean full = this.renderType == ShapeRenderType.FULL_BLOCK;
        boolean outer = this.renderType == ShapeRenderType.OUTER_EDGE;
        boolean inner = this.renderType == ShapeRenderType.INNER_EDGE;
        LayerRange range = this.layerRange;

        for (long posLong : positions)
        {
            int x = BlockPos.unpackLongX(posLong);
            int y = BlockPos.unpackLongY(posLong);
            int z = BlockPos.unpackLongZ(posLong);

            if (range.isPositionWithinRange(x, y, z))
            {
                for (Direction side : sides)
                {
                    long adjPosLong = BlockPos.offset(posLong, side);

                    if (positions.contains(adjPosLong) == false)
                    {
                        boolean render = full;

                        if (full == false)
                        {
                            int adjX = BlockPos.unpackLongX(adjPosLong);
                            int adjY = BlockPos.unpackLongY(adjPosLong);
                            int adjZ = BlockPos.unpackLongZ(adjPosLong);
                            boolean onOrIn = test.isInsideOrCloserThan(adjX, adjY, adjZ, side);
                            render = ((outer && onOrIn == false) || (inner && onOrIn));
                        }

                        if (render)
                        {
                            drawBlockSpaceSideBatchedQuads(posLong, side, color, expand, cameraPos, BUFFER_1);
                        }
                    }
                }
            }
        }
    }

    /**
     * Assumes a BufferBuilder in GL_QUADS mode has been initialized
     */
    public static void drawBlockSpaceSideBatchedQuads(long posLong,
                                                      Direction side,
                                                      Color4f color,
                                                      double expand,
                                                      Vec3d cameraPos,
                                                      BufferBuilder buffer)
    {
        int x = BlockPos.unpackLongX(posLong);
        int y = BlockPos.unpackLongY(posLong);
        int z = BlockPos.unpackLongZ(posLong);
        double offsetX = x - cameraPos.x;
        double offsetY = y - cameraPos.y;
        double offsetZ = z - cameraPos.z;
        double minX = offsetX - expand;
        double minY = offsetY - expand;
        double minZ = offsetZ - expand;
        double maxX = offsetX + expand + 1;
        double maxY = offsetY + expand + 1;
        double maxZ = offsetZ + expand + 1;

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
