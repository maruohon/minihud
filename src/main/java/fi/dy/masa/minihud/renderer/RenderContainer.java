package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import com.google.gson.JsonObject;
import com.mojang.blaze3d.systems.RenderSystem;
import org.joml.Matrix4f;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.config.RendererToggle;

public class RenderContainer
{
    public static final RenderContainer INSTANCE = new RenderContainer();

    private final List<OverlayRendererBase> renderers = new ArrayList<>();
    protected boolean resourcesAllocated;
    protected int countActive;

    private RenderContainer()
    {
        this.addRenderer(OverlayRendererBeaconRange.INSTANCE);
        this.addRenderer(OverlayRendererBiomeBorders.INSTANCE);
        this.addRenderer(new OverlayRendererBlockGrid());
        this.addRenderer(OverlayRendererConduitRange.INSTANCE);
        this.addRenderer(OverlayRendererLightLevel.INSTANCE);
        this.addRenderer(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_FIXED));
        this.addRenderer(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER));
        this.addRenderer(new OverlayRendererRegion());
        this.addRenderer(new OverlayRendererSlimeChunks());
        this.addRenderer(new OverlayRendererSpawnableColumnHeights());
        this.addRenderer(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL));
        this.addRenderer(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER));
        this.addRenderer(OverlayRendererStructures.INSTANCE);
    }

    public void addRenderer(OverlayRendererBase renderer)
    {
        if (this.resourcesAllocated)
        {
            renderer.allocateGlResources();
        }

        this.renderers.add(renderer);
    }

    public void removeRenderer(OverlayRendererBase renderer)
    {
        this.renderers.remove(renderer);

        if (this.resourcesAllocated)
        {
            renderer.deleteGlResources();
        }
    }

    public void render(Entity entity, MatrixStack matrixStack, Matrix4f projMatrix, MinecraftClient mc)
    {
        Vec3d cameraPos = mc.gameRenderer.getCamera().getPos();

        this.update(cameraPos, entity, mc);
        this.draw(cameraPos, matrixStack, projMatrix, mc);
    }

    protected void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        mc.getProfiler().push(() -> "RenderContainer#update()");
        this.allocateResourcesIfNeeded();
        this.countActive = 0;

        for (OverlayRendererBase renderer : this.renderers)
        {
            mc.getProfiler().push(() -> renderer.getClass().getName());

            if (renderer.shouldRender(mc))
            {
                if (renderer.needsUpdate(entity, mc))
                {
                    renderer.lastUpdatePos = PositionUtils.getEntityBlockPos(entity);
                    renderer.setUpdatePosition(cameraPos);
                    renderer.update(cameraPos, entity, mc);
                }

                ++this.countActive;
            }

            mc.getProfiler().pop();
        }

        mc.getProfiler().pop();
    }

    protected void draw(Vec3d cameraPos, MatrixStack matrixStack, Matrix4f projMatrix, MinecraftClient mc)
    {
        if (this.resourcesAllocated && this.countActive > 0)
        {
            mc.getProfiler().push(() -> "RenderContainer#draw()");

            RenderSystem.disableCull();
            RenderSystem.enableDepthTest();
            RenderSystem.depthMask(false);
            RenderSystem.polygonOffset(-3f, -3f);
            RenderSystem.enablePolygonOffset();

            fi.dy.masa.malilib.render.RenderUtils.setupBlend();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);

            for (IOverlayRenderer renderer : this.renderers)
            {
                mc.getProfiler().push(() -> renderer.getClass().getName());

                if (renderer.shouldRender(mc))
                {
                    Vec3d updatePos = renderer.getUpdatePosition();
                    matrixStack.push();
                    matrixStack.translate(updatePos.x - cameraPos.x, updatePos.y - cameraPos.y, updatePos.z - cameraPos.z);

                    renderer.draw(matrixStack, projMatrix);

                    matrixStack.pop();
                }

                mc.getProfiler().pop();
            }

            RenderSystem.polygonOffset(0f, 0f);
            RenderSystem.disablePolygonOffset();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);
            RenderSystem.disableBlend();
            RenderSystem.enableDepthTest();
            RenderSystem.enableCull();
            RenderSystem.depthMask(true);

            mc.getProfiler().pop();
        }
    }

    protected void allocateResourcesIfNeeded()
    {
        if (this.resourcesAllocated == false)
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
