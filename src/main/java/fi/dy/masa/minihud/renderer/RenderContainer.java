package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GLX;
import com.mojang.blaze3d.platform.GlStateManager;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gl.GlBuffer;
import net.minecraft.client.render.VertexFormatElement;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;

public class RenderContainer
{
    protected final List<IOverlayRenderer> renderers = new ArrayList<>();
    protected boolean resourcesAllocated;
    protected boolean useVbo;

    public RenderContainer()
    {
        this.init();
    }

    public void init()
    {
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

        this.allocateGlResources();
    }

    public void render(Entity entity, MinecraftClient mc, float partialTicks)
    {
        this.update(entity, mc);
        this.draw(entity, mc, partialTicks);
    }

    protected void update(Entity entity, MinecraftClient mc)
    {
        this.checkVideoSettings();

        for (int i = 0; i < this.renderers.size(); ++i)
        {
            IOverlayRenderer renderer = this.renderers.get(i);

            if (renderer.shouldRender(mc) && renderer.needsUpdate(entity, mc))
            {
                //System.out.printf("plop update\n");
                renderer.update(entity, mc);
            }
        }
    }

    protected void draw(Entity entity, MinecraftClient mc, float partialTicks)
    {
        if (this.resourcesAllocated)
        {
            GlStateManager.pushMatrix();

            GlStateManager.disableTexture();
            //GlStateManager.matrixMode(GL11.GL_MODELVIEW);
            GlStateManager.alphaFunc(GL11.GL_GREATER, 0.01F);
            GlStateManager.disableCull();
            GlStateManager.disableLighting();
            GlStateManager.depthMask(false);
            GlStateManager.enablePolygonOffset();
            GlStateManager.polygonOffset(-0.1f, -0.8f);
            GlStateManager.enableBlend();
            fi.dy.masa.malilib.render.RenderUtils.setupBlend();
            GlStateManager.color4f(1f, 1f, 1f, 1f);

            if (GLX.useVbo())
            {
                GlStateManager.enableClientState(GL11.GL_VERTEX_ARRAY);
                GlStateManager.enableClientState(GL11.GL_COLOR_ARRAY);
            }

            double dx = entity.prevRenderX + (entity.x - entity.prevRenderX) * partialTicks;
            double dy = entity.prevRenderY + (entity.y - entity.prevRenderY) * partialTicks;
            double dz = entity.prevRenderZ + (entity.z - entity.prevRenderZ) * partialTicks;

            GlStateManager.translated((float) -dx, (float) -dy, (float) -dz);

            for (int i = 0; i < this.renderers.size(); ++i)
            {
                IOverlayRenderer renderer = this.renderers.get(i);

                if (renderer.shouldRender(mc))
                {
                    renderer.draw();
                }
            }

            if (GLX.useVbo())
            {
                GlBuffer.unbind();
                GlStateManager.clearCurrentColor();

                for (VertexFormatElement element : VertexFormats.POSITION_COLOR.getElements())
                {
                    VertexFormatElement.Type usage = element.getType();

                    switch (usage)
                    {
                        case POSITION:
                            GlStateManager.disableClientState(GL11.GL_VERTEX_ARRAY);
                            break;
                        case COLOR:
                            GlStateManager.disableClientState(GL11.GL_COLOR_ARRAY);
                            GlStateManager.clearCurrentColor();
                        default:
                    }
                }

            }

            GlStateManager.color4f(1f, 1f, 1f, 1f);
            GlStateManager.disableBlend();
            GlStateManager.enableDepthTest();
            GlStateManager.enableLighting();
            GlStateManager.enableCull();
            GlStateManager.depthMask(true);
            GlStateManager.polygonOffset(0f, 0f);
            GlStateManager.disablePolygonOffset();
            GlStateManager.enableTexture();
            GlStateManager.popMatrix();
        }
    }

    protected void checkVideoSettings()
    {
        boolean vboLast = this.useVbo;
        this.useVbo = GLX.useVbo();

        if (vboLast != this.useVbo || this.resourcesAllocated == false)
        {
            this.deleteGlResources();
            this.init();
        }
    }

    protected void allocateGlResources()
    {
        if (this.resourcesAllocated == false)
        {
            for (int i = 0; i < this.renderers.size(); ++i)
            {
                IOverlayRenderer renderer = this.renderers.get(i);
                renderer.allocateGlResources();
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
                IOverlayRenderer renderer = this.renderers.get(i);
                renderer.deleteGlResources();
            }

            this.renderers.clear();
            this.resourcesAllocated = false;
        }
    }
}
