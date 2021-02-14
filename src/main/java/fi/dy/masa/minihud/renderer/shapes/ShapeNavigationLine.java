package fi.dy.masa.minihud.renderer.shapes;

import java.util.ArrayList;
import java.util.List;

import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;

public class ShapeNavigationLine extends ShapeBase
{
    protected Vec3d destination = Vec3d.ZERO;
    protected Vec3d lastUpdatePos = Vec3d.ZERO;
    protected long lastUpdateTime;
    private static float WIDTH = 0.0001f;

    public ShapeNavigationLine()
    {
        super(ShapeType.NAVIGATION_LINE, Configs.Colors.SHAPE_NAVIGATION_LINE.getColor());

        Entity entity = EntityUtils.getCameraEntity();

        if (entity != null)
        {
            Vec3d center = entity.getPos();
            center = new Vec3d(Math.floor(center.x) + 0.5, Math.floor(center.y), Math.floor(center.z) + 0.5);
            this.setDestination(this.mc.player.getPos());
        }
        else
        {
            this.setDestination(Vec3d.ZERO);
        }
        this.setNeedsUpdate();
    }

    public void setDestination(Vec3d destination)
    {
        this.destination = destination;
        this.setNeedsUpdate();
    }

    public Vec3d getDestination() {
        return this.destination;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        RenderSystem.lineWidth(100f);
        this.renderLine(cameraPos);
        RenderSystem.lineWidth(1f);

        this.lastUpdatePos = entity.getPos();
        this.lastUpdateTime = System.currentTimeMillis();
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_LINES);
    }

    @Override
    public void draw(MatrixStack matrixStack)
    {
        this.preRender();

        this.renderObjects.get(0).draw(matrixStack);

        // Render the lines as quads with glPolygonMode(GL_LINE)
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE);
        RenderSystem.disableBlend();
        this.renderObjects.get(0).draw(matrixStack);
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL);
        RenderSystem.enableBlend();
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        if (obj != null) {
            obj.add("destination", JsonUtils.vec3dToJson(this.destination));
            obj.add("color", new JsonPrimitive(this.color.intValue));
        }
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        Vec3d destination = JsonUtils.vec3dFromJson(obj, "destination");

        if (destination != null)
        {
            this.setDestination(destination);
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
        Vec3d c = this.destination;
        lines.add(StringUtils.translate("minihud.gui.label.destination_value", String.format("x: %.2f, y: %.2f, z: %.2f", c.x, c.y, c.z)));

        return lines;
    }

    protected void renderLine(Vec3d cameraPos)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);

        // playerDiff should be playerPos.subtract(cameraPos),
        // so it stays in the player, even if the camera mode (F5) is changed
        // but I don't know why its very laggy to call this.mc.player.getPos
        Vec3d playerDiff = new Vec3d(0d, -1d, 0d);
        Vec3d destDiff = this.destination.subtract(cameraPos);

        // perpendicular offset that will give the width of the line
        Vec3d offset = new Vec3d(destDiff.getX(), 0d, destDiff.getZ())
                .normalize()
                .multiply(WIDTH / 2)
                .rotateY((float) Math.toRadians(90));

        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        BUFFER_1.vertex(destDiff.getX() - offset.getX(), destDiff.getY(), destDiff.getZ()  - offset.getZ()).color(color.r, color.g, color.b, color.a).next();
        BUFFER_1.vertex(destDiff.getX() + offset.getX(), destDiff.getY(), destDiff.getZ() + offset.getZ()).color(color.r, color.g, color.b, color.a).next();
        BUFFER_1.vertex(playerDiff.getX() + offset.getX(), playerDiff.getY(), playerDiff.getZ() + offset.getZ()).color(color.r, color.g, color.b, color.a).next();
        BUFFER_1.vertex(playerDiff.getX() - offset.getX(), playerDiff.getY(), playerDiff.getZ() - offset.getZ()).color(color.r, color.g, color.b, color.a).next();

        BUFFER_1.end();

        renderQuads.uploadData(BUFFER_1);
    }
}
