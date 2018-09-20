package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.client.renderer.vertex.VertexFormatElement;
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
        this.renderers.add(new OverlayRendererRegion());
        this.renderers.add(new OverlayRendererSlimeChunks());
        this.renderers.add(new OverlayRendererSpawnableColumnHeights());
        this.renderers.add(new OverlayRendererSpawnChunks(
                RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL,
                Configs.Colors.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getIntegerValue(),
                Configs.Colors.SPAWN_REAL_LAZY_OVERLAY_COLOR.getIntegerValue()));
        this.renderers.add(new OverlayRendererSpawnChunks(
                RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER,
                Configs.Colors.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getIntegerValue(),
                Configs.Colors.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getIntegerValue()));

        this.allocateGlResources();
    }

    public void render(Entity entity, Minecraft mc, float partialTicks)
    {
        this.update(entity, mc);
        this.draw(entity, mc, partialTicks);
    }

    protected void update(Entity entity, Minecraft mc)
    {
        this.checkVideoSettings();

        for (int i = 0; i < this.renderers.size(); ++i)
        {
            IOverlayRenderer renderer = this.renderers.get(i);

            if (renderer.shouldRender(mc) && renderer.needsUpdate(entity, mc))
            {
                renderer.update(entity, mc);
            }
        }
    }

    protected void draw(Entity entity, Minecraft mc, float partialTicks)
    {
        if (this.resourcesAllocated)
        {
            GlStateManager.pushMatrix();

            GlStateManager.disableTexture2D();
            //GlStateManager.matrixMode(GL11.GL_MODELVIEW);
            GlStateManager.alphaFunc(GL11.GL_GREATER, 0.01F);
            GlStateManager.disableCull();
            GlStateManager.disableLighting();
            GlStateManager.depthMask(false);
            GlStateManager.enablePolygonOffset();
            GlStateManager.doPolygonOffset(-0.1f, -0.8f);
            GlStateManager.enableBlend();
            GlStateManager.tryBlendFuncSeparate(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA, GlStateManager.SourceFactor.ONE, GlStateManager.DestFactor.ZERO);
            GlStateManager.color(1f, 1f, 1f, 1f);

            if (OpenGlHelper.useVbo())
            {
                GlStateManager.glEnableClientState(GL11.GL_VERTEX_ARRAY);
                GlStateManager.glEnableClientState(GL11.GL_COLOR_ARRAY);
            }

            double dx = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
            double dy = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
            double dz = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;

            GlStateManager.translate((float) -dx, (float) -dy, (float) -dz);

            for (int i = 0; i < this.renderers.size(); ++i)
            {
                IOverlayRenderer renderer = this.renderers.get(i);

                if (renderer.shouldRender(mc))
                {
                    renderer.draw();
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

            GlStateManager.color(1f, 1f, 1f, 1f);
            GlStateManager.disableBlend();
            GlStateManager.enableDepth();
            GlStateManager.enableCull();
            GlStateManager.doPolygonOffset(0f, 0f);
            GlStateManager.disablePolygonOffset();
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
