package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import com.google.gson.JsonObject;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Matrix4f;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;

public class RenderContainer
{
    public static final RenderContainer INSTANCE = new RenderContainer();

    private final List<OverlayRendererBase> renderers = new ArrayList<>();
    protected boolean resourcesAllocated;
    protected int countActive;

    private RenderContainer()
    {
        this.addRenderer(new OverlayRendererBeaconRange());
        this.addRenderer(new OverlayRendererBlockGrid());
        this.addRenderer(new OverlayRendererLightLevel());
        this.addRenderer(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_FIXED));
        this.addRenderer(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER));
        this.addRenderer(new OverlayRendererRegion());
        this.addRenderer(new OverlayRendererSlimeChunks());
        this.addRenderer(new OverlayRendererSpawnableColumnHeights());
        this.addRenderer(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL));
        this.addRenderer(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER));
        this.addRenderer(OverlayRendererStructures.INSTANCE);
    }

    private void addRenderer(OverlayRendererBase renderer)
    {
        if (this.resourcesAllocated)
        {
            renderer.allocateGlResources();
        }

        this.renderers.add(renderer);
    }

    public void addShapeRenderer(ShapeBase renderer)
    {
        this.addRenderer(renderer);
    }

    public void removeShapeRenderer(ShapeBase renderer)
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
        this.allocateResourcesIfNeeded();
        this.countActive = 0;

        for (int i = 0; i < this.renderers.size(); ++i)
        {
            OverlayRendererBase renderer = this.renderers.get(i);

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
        }
    }

    protected void draw(Vec3d cameraPos, MatrixStack matrixStack, Matrix4f projMatrix, MinecraftClient mc)
    {
        if (this.resourcesAllocated && this.countActive > 0)
        {
            RenderSystem.disableTexture();
            RenderSystem.disableCull();
            RenderSystem.enableDepthTest();
            RenderSystem.depthMask(false);
            RenderSystem.polygonOffset(-3f, -3f);
            RenderSystem.enablePolygonOffset();

            fi.dy.masa.malilib.render.RenderUtils.setupBlend();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);

            for (IOverlayRenderer renderer : this.renderers)
            {
                if (renderer.shouldRender(mc))
                {
                    Vec3d updatePos = renderer.getUpdatePosition();
                    matrixStack.push();
                    matrixStack.translate(updatePos.x - cameraPos.x, updatePos.y - cameraPos.y, updatePos.z - cameraPos.z);

                    renderer.draw(matrixStack, projMatrix);

                    matrixStack.pop();
                }
            }

            RenderSystem.polygonOffset(0f, 0f);
            RenderSystem.disablePolygonOffset();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);
            RenderSystem.disableBlend();
            RenderSystem.enableDepthTest();
            RenderSystem.enableCull();
            RenderSystem.depthMask(true);
            RenderSystem.enableTexture();
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
            for (int i = 0; i < this.renderers.size(); ++i)
            {
                this.renderers.get(i).allocateGlResources();
            }

            this.resourcesAllocated = true;
        }
    }

    protected void deleteGlResources()
    {
        if (this.resourcesAllocated)
        {
            for (int i = 0; i < this.renderers.size(); ++i)
            {
                this.renderers.get(i).deleteGlResources();
            }

            this.resourcesAllocated = false;
        }
    }

    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();

        for (int i = 0; i < this.renderers.size(); ++i)
        {
            OverlayRendererBase renderer = this.renderers.get(i);
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
        for (int i = 0; i < this.renderers.size(); ++i)
        {
            OverlayRendererBase renderer = this.renderers.get(i);
            String id = renderer.getSaveId();

            if (id.isEmpty() == false && JsonUtils.hasObject(obj, id))
            {
                renderer.fromJson(obj.get(id).getAsJsonObject());
            }
        }
    }
}
