package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.client.renderer.vertex.VertexFormatElement;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;

public class RenderContainer
{
    public static final RenderContainer INSTANCE = new RenderContainer();

    protected final List<OverlayRendererBase> renderers = new ArrayList<>();
    protected boolean resourcesAllocated;
    protected boolean useVbo;
    protected int countActive;

    private RenderContainer()
    {
        this.renderers.add(new OverlayRendererBeaconRange());
        this.renderers.add(new OverlayRendererBlockGrid());
        this.renderers.add(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_FIXED));
        this.renderers.add(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER));
        this.renderers.add(new OverlayRendererRegion());
        this.renderers.add(new OverlayRendererSlimeChunks());
        this.renderers.add(new OverlayRendererSpawnableColumnHeights());
        this.renderers.add(new OverlayRendererSpawnableChunks(RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED));
        this.renderers.add(new OverlayRendererSpawnableChunks(RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_PLAYER));
        this.renderers.add(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL));
        this.renderers.add(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER));
        this.renderers.add(new OverlayRendererStructures());
    }

    public void addShapeRenderer(ShapeBase renderer)
    {
        if (this.resourcesAllocated)
        {
            renderer.allocateGlResources();
        }

        this.renderers.add(renderer);
    }

    public void removeShapeRenderer(ShapeBase renderer)
    {
        this.renderers.remove(renderer);

        if (this.resourcesAllocated)
        {
            renderer.deleteGlResources();
        }
    }

    public void render(Entity entity, Minecraft mc, float partialTicks)
    {
        double x = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
        double y = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
        double z = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;
        Vec3d cameraPos = new Vec3d(x, y, z);

        this.update(cameraPos, entity, mc);
        this.draw(cameraPos, mc);
    }

    protected void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        this.checkVideoSettings();
        this.countActive = 0;

        for (int i = 0; i < this.renderers.size(); ++i)
        {
            OverlayRendererBase renderer = this.renderers.get(i);

            if (renderer.shouldRender(mc))
            {
                if (renderer.needsUpdate(entity, mc))
                {
                    renderer.lastUpdatePos = new BlockPos(entity);
                    renderer.setUpdatePosition(cameraPos);
                    renderer.update(cameraPos, entity, mc);
                }

                ++this.countActive;
            }
        }
    }

    protected void draw(Vec3d cameraPos, Minecraft mc)
    {
        if (this.resourcesAllocated && this.countActive > 0)
        {
            GlStateManager.pushMatrix();

            GlStateManager.disableTexture2D();
            GlStateManager.alphaFunc(GL11.GL_GREATER, 0.01F);
            GlStateManager.disableCull();
            GlStateManager.disableLighting();
            GlStateManager.depthMask(false);
            GlStateManager.doPolygonOffset(-3f, -3f);
            GlStateManager.enablePolygonOffset();
            fi.dy.masa.malilib.render.RenderUtils.setupBlend();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);

            if (OpenGlHelper.useVbo())
            {
                GlStateManager.glEnableClientState(GL11.GL_VERTEX_ARRAY);
                GlStateManager.glEnableClientState(GL11.GL_COLOR_ARRAY);
            }

            double cx = cameraPos.x;
            double cy = cameraPos.y;
            double cz = cameraPos.z;

            for (IOverlayRenderer renderer : this.renderers)
            {
                if (renderer.shouldRender(mc))
                {
                    Vec3d updatePos = renderer.getUpdatePosition();
                    GlStateManager.pushMatrix();
                    GlStateManager.translate(updatePos.x - cx, updatePos.y - cy, updatePos.z - cz);

                    renderer.draw();

                    GlStateManager.popMatrix();
                }
            }

            if (OpenGlHelper.useVbo())
            {
                OpenGlHelper.glBindBuffer(OpenGlHelper.GL_ARRAY_BUFFER, 0);
                GlStateManager.resetColor();

                for (VertexFormatElement element : DefaultVertexFormats.POSITION_COLOR.getElements())
                {
                    VertexFormatElement.EnumUsage usage = element.getUsage();

                    switch (usage)
                    {
                        case POSITION:
                            GlStateManager.glDisableClientState(GL11.GL_VERTEX_ARRAY);
                            break;
                        case COLOR:
                            GlStateManager.glDisableClientState(GL11.GL_COLOR_ARRAY);
                            GlStateManager.resetColor();
                        default:
                    }
                }
            }

            GlStateManager.doPolygonOffset(0f, 0f);
            GlStateManager.disablePolygonOffset();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);
            GlStateManager.disableBlend();
            GlStateManager.enableDepth();
            GlStateManager.enableCull();
            GlStateManager.depthMask(true);
            GlStateManager.enableTexture2D();
            GlStateManager.popMatrix();
        }
    }

    protected void checkVideoSettings()
    {
        boolean vboLast = this.useVbo;
        this.useVbo = OpenGlHelper.useVbo();

        if (vboLast != this.useVbo || this.resourcesAllocated == false)
        {
            this.deleteGlResources();
            this.allocateGlResources();
        }
    }

    protected void allocateGlResources()
    {
        if (this.resourcesAllocated == false)
        {
            for (OverlayRendererBase renderer : this.renderers)
            {
                renderer.allocateGlResources();
            }

            this.resourcesAllocated = true;
        }
    }

    protected void deleteGlResources()
    {
        if (this.resourcesAllocated)
        {
            for (OverlayRendererBase renderer : this.renderers)
            {
                renderer.deleteGlResources();
            }

            this.resourcesAllocated = false;
        }
    }

    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();

        for (OverlayRendererBase renderer : this.renderers)
        {
            String id = renderer.getSaveId();

            if (id.isEmpty() == false)
            {
                obj.add(id, renderer.toJson());
            }
        }

        return obj;
    }

    public void fromJson(JsonObject obj)
    {
        for (OverlayRendererBase renderer : this.renderers)
        {
            String id = renderer.getSaveId();

            if (id.isEmpty() == false && JsonUtils.hasObject(obj, id))
            {
                renderer.fromJson(obj.get(id).getAsJsonObject());
            }
        }
    }
}
