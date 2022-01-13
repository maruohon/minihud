package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
import com.google.gson.JsonObject;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Box;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;

public class ShapeBox extends ShapeBase
{
    public static final Box EMPTY_BOX = new Box(0, 0, 0, 0, 0, 0);

    protected Box box = EMPTY_BOX;
    protected Box renderPerimeter = EMPTY_BOX;
    protected int enabledSidesMask = 0x3F;
    protected boolean gridEnabled = true;
    protected Vec3d gridSize = new Vec3d(16.0, 16.0, 16.0);
    protected Vec3d gridStartOffset = Vec3d.ZERO;
    protected Vec3d gridEndOffset = Vec3d.ZERO;

    public ShapeBox()
    {
        super(ShapeType.BOX, Configs.Colors.SHAPE_BOX.getColor());
    }

    public Box getBox()
    {
        return this.box;
    }

    public int getEnabledSidesMask()
    {
        return this.enabledSidesMask;
    }

    public boolean isGridEnabled()
    {
        return this.gridEnabled;
    }

    public Vec3d getGridSize()
    {
        return this.gridSize;
    }

    public Vec3d getGridStartOffset()
    {
        return this.gridStartOffset;
    }

    public Vec3d getGridEndOffset()
    {
        return this.gridEndOffset;
    }

    public void setBox(Box box)
    {
        this.box = box;

        double margin = MinecraftClient.getInstance().options.viewDistance * 16 * 2;
        this.renderPerimeter = box.expand(margin);
        this.setNeedsUpdate();
    }

    public void setEnabledSidesMask(int enabledSidesMask)
    {
        this.enabledSidesMask = enabledSidesMask;
        this.setNeedsUpdate();
    }

    public void toggleGridEnabled()
    {
        this.gridEnabled = ! this.gridEnabled;
        this.setNeedsUpdate();
    }

    public void setGridSize(Vec3d gridSize)
    {
        double x = MathHelper.clamp(gridSize.x, 0.5, 1024);
        double y = MathHelper.clamp(gridSize.y, 0.5, 1024);
        double z = MathHelper.clamp(gridSize.z, 0.5, 1024);
        this.gridSize = new Vec3d(x, y, z);
        this.setNeedsUpdate();
    }

    public void setGridStartOffset(Vec3d gridStartOffset)
    {
        double x = MathHelper.clamp(gridStartOffset.x, 0.0, 1024);
        double y = MathHelper.clamp(gridStartOffset.y, 0.0, 1024);
        double z = MathHelper.clamp(gridStartOffset.z, 0.0, 1024);
        this.gridStartOffset = new Vec3d(x, y, z);
        this.setNeedsUpdate();
    }

    public void setGridEndOffset(Vec3d gridEndOffset)
    {
        double x = MathHelper.clamp(gridEndOffset.x, 0.0, 1024);
        double y = MathHelper.clamp(gridEndOffset.y, 0.0, 1024);
        double z = MathHelper.clamp(gridEndOffset.z, 0.0, 1024);
        this.gridEndOffset = new Vec3d(x, y, z);
        this.setNeedsUpdate();
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        Entity entity = EntityUtils.getCameraEntity();
        return super.shouldRender(mc) && entity != null && this.renderPerimeter.contains(entity.getPos());
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        Box box = this.box.offset(-cameraPos.x, -cameraPos.y, -cameraPos.z);

        this.renderBox(box);

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.needsUpdate = false;
    }

    protected void renderBox(Box box)
    {
        for (Direction side : PositionUtils.ALL_DIRECTIONS)
        {
            if (isSideEnabled(side, this.enabledSidesMask))
            {
                renderBoxSideQuad(box, side, this.color, BUFFER_1);
            }
        }

        Color4f color = Color4f.fromColor(this.color, 1f);
        renderBoxEnabledEdgeLines(box, color, this.enabledSidesMask, BUFFER_2);

        if (this.gridEnabled)
        {
            this.renderGridLines(box, color);
        }
    }

    protected void renderGridLines(Box box, Color4f color)
    {
        if (isSideEnabled(Direction.DOWN, this.enabledSidesMask))
        {
            this.renderGridLinesY(box, box.minY, color, BUFFER_2);
        }

        if (isSideEnabled(Direction.UP, this.enabledSidesMask))
        {
            this.renderGridLinesY(box, box.maxY, color, BUFFER_2);
        }

        if (isSideEnabled(Direction.NORTH, this.enabledSidesMask))
        {
            this.renderGridLinesZ(box, box.minZ, color, BUFFER_2);
        }

        if (isSideEnabled(Direction.SOUTH, this.enabledSidesMask))
        {
            this.renderGridLinesZ(box, box.maxZ, color, BUFFER_2);
        }

        if (isSideEnabled(Direction.WEST, this.enabledSidesMask))
        {
            this.renderGridLinesX(box, box.minX, color, BUFFER_2);
        }

        if (isSideEnabled(Direction.EAST, this.enabledSidesMask))
        {
            this.renderGridLinesX(box, box.maxX, color, BUFFER_2);
        }
    }

    protected void renderGridLinesX(Box box, double x, Color4f color, BufferBuilder buffer)
    {
        double end = box.maxY - this.gridEndOffset.y;
        double min = box.minZ + this.gridStartOffset.z;
        double max = box.maxZ - this.gridEndOffset.z;

        for (double y = box.minY + this.gridStartOffset.y; y <= end; y += this.gridSize.y)
        {
            buffer.vertex(x, y, min).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(x, y, max).color(color.r, color.g, color.b, color.a).next();
        }

        end = box.maxZ - this.gridEndOffset.z;
        min = box.minY + this.gridStartOffset.y;
        max = box.maxY - this.gridEndOffset.y;

        for (double z = box.minZ + this.gridStartOffset.z; z <= end; z += this.gridSize.z)
        {
            buffer.vertex(x, min, z).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(x, max, z).color(color.r, color.g, color.b, color.a).next();
        }
    }

    protected void renderGridLinesY(Box box, double y, Color4f color, BufferBuilder buffer)
    {
        double end = box.maxX - this.gridEndOffset.x;
        double min = box.minZ + this.gridStartOffset.z;
        double max = box.maxZ - this.gridEndOffset.z;

        for (double x = box.minX + this.gridStartOffset.x; x <= end; x += this.gridSize.x)
        {
            buffer.vertex(x, y, min).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(x, y, max).color(color.r, color.g, color.b, color.a).next();
        }

        end = box.maxZ - this.gridEndOffset.z;
        min = box.minX + this.gridStartOffset.x;
        max = box.maxX - this.gridEndOffset.x;

        for (double z = box.minZ + this.gridStartOffset.z; z <= end; z += this.gridSize.z)
        {
            buffer.vertex(min, y, z).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(max, y, z).color(color.r, color.g, color.b, color.a).next();
        }
    }

    protected void renderGridLinesZ(Box box, double z, Color4f color, BufferBuilder buffer)
    {
        double end = box.maxX - this.gridEndOffset.x;
        double min = box.minY + this.gridStartOffset.y;
        double max = box.maxY - this.gridEndOffset.y;

        for (double x = box.minX + this.gridStartOffset.x; x <= end; x += this.gridSize.x)
        {
            buffer.vertex(x, min, z).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(x, max, z).color(color.r, color.g, color.b, color.a).next();
        }

        end = box.maxY - this.gridEndOffset.y;
        min = box.minX + this.gridStartOffset.x;
        max = box.maxX - this.gridEndOffset.x;

        for (double y = box.minY + this.gridStartOffset.y; y <= end; y += this.gridSize.y)
        {
            buffer.vertex(min, y, z).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(max, y, z).color(color.r, color.g, color.b, color.a).next();
        }
    }

    public boolean isSideEnabled(Direction side)
    {
        return isSideEnabled(side, this.enabledSidesMask);
    }

    public static boolean isSideEnabled(Direction side, int enabledSidesMask)
    {
        return (enabledSidesMask & (1 << side.getId())) != 0;
    }

    public static void renderBoxSideQuad(Box box, Direction side, Color4f color, BufferBuilder buffer)
    {
        switch (side)
        {
            case DOWN:
                buffer.vertex(box.minX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.minX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case UP:
                buffer.vertex(box.minX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.minX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case NORTH:
                buffer.vertex(box.minX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.minX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case SOUTH:
                buffer.vertex(box.minX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.minX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case WEST:
                buffer.vertex(box.minX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.minX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.minX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.minX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case EAST:
                buffer.vertex(box.maxX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(box.maxX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
                break;
        }
    }

    public static void renderBoxEnabledEdgeLines(Box box, Color4f color, int enabledSidesMask, BufferBuilder buffer)
    {
        boolean down  = isSideEnabled(Direction.DOWN,   enabledSidesMask);
        boolean up    = isSideEnabled(Direction.UP,     enabledSidesMask);
        boolean north = isSideEnabled(Direction.NORTH,  enabledSidesMask);
        boolean south = isSideEnabled(Direction.SOUTH,  enabledSidesMask);
        boolean west  = isSideEnabled(Direction.WEST,   enabledSidesMask);
        boolean east  = isSideEnabled(Direction.EAST,   enabledSidesMask);

        // Lines along the x-axis
        if (down || north)
        {
            buffer.vertex(box.minX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (up || north)
        {
            buffer.vertex(box.minX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (down || south)
        {
            buffer.vertex(box.minX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (up || south)
        {
            buffer.vertex(box.minX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }

        // Lines along the z-axis
        if (down || west)
        {
            buffer.vertex(box.minX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.minX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (up || west)
        {
            buffer.vertex(box.minX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.minX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (down || east)
        {
            buffer.vertex(box.maxX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (up || east)
        {
            buffer.vertex(box.maxX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }

        // Lines along the y-axis
        if (north || west)
        {
            buffer.vertex(box.minX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.minX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (south || west)
        {
            buffer.vertex(box.minX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.minX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (north || east)
        {
            buffer.vertex(box.maxX, box.minY, box.minZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.maxY, box.minZ).color(color.r, color.g, color.b, color.a).next();
        }

        if (south || east)
        {
            buffer.vertex(box.maxX, box.minY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
            buffer.vertex(box.maxX, box.maxY, box.maxZ).color(color.r, color.g, color.b, color.a).next();
        }
    }

    @Override
    public List<String> getWidgetHoverLines()
    {
        List<String> lines = super.getWidgetHoverLines();
        Box box = this.box;
        lines.add(StringUtils.translate("minihud.gui.label.shape.box.min_corner", box.minX, box.minY, box.minZ));
        lines.add(StringUtils.translate("minihud.gui.label.shape.box.max_corner", box.maxX, box.maxY, box.maxZ));
        return lines;
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();

        obj.addProperty("enabled_sides", this.enabledSidesMask);
        obj.addProperty("grid_enabled", this.gridEnabled);
        obj.add("grid_size",         JsonUtils.vec3dToJson(this.gridSize));
        obj.add("grid_start_offset", JsonUtils.vec3dToJson(this.gridStartOffset));
        obj.add("grid_end_offset",   JsonUtils.vec3dToJson(this.gridEndOffset));

        obj.addProperty("minX", this.box.minX);
        obj.addProperty("minY", this.box.minY);
        obj.addProperty("minZ", this.box.minZ);
        obj.addProperty("maxX", this.box.maxX);
        obj.addProperty("maxY", this.box.maxY);
        obj.addProperty("maxZ", this.box.maxZ);

        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        this.enabledSidesMask = JsonUtils.getIntegerOrDefault(obj, "enabled_sides", 0x3F);
        this.gridEnabled     = JsonUtils.getBooleanOrDefault(obj, "grid_enabled", true);
        this.gridSize        = JsonUtils.vec3dFromJson(obj, "grid_size");
        this.gridStartOffset = JsonUtils.vec3dFromJson(obj, "grid_start_offset");
        this.gridEndOffset   = JsonUtils.vec3dFromJson(obj, "grid_end_offset");

        if (this.gridSize == null)        { this.gridSize = new Vec3d(16.0, 16.0, 16.0); }
        if (this.gridStartOffset == null) { this.gridStartOffset = Vec3d.ZERO; }
        if (this.gridEndOffset == null)   { this.gridEndOffset = Vec3d.ZERO; }

        double minX = JsonUtils.getDoubleOrDefault(obj, "minX", 0);
        double minY = JsonUtils.getDoubleOrDefault(obj, "minY", 0);
        double minZ = JsonUtils.getDoubleOrDefault(obj, "minZ", 0);
        double maxX = JsonUtils.getDoubleOrDefault(obj, "maxX", 0);
        double maxY = JsonUtils.getDoubleOrDefault(obj, "maxY", 0);
        double maxZ = JsonUtils.getDoubleOrDefault(obj, "maxZ", 0);

        this.setBox(new Box(minX, minY, minZ, maxX, maxY, maxZ));
    }
}
