package fi.dy.masa.minihud.renderer;

import java.util.Collections;
import org.lwjgl.opengl.GL11;

import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBlock;

import malilib.render.RenderUtils;
import malilib.render.ShapeRenderUtils;
import malilib.render.TextRenderUtils;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.MiscUtils;

public class OverlayRenderer
{
    private static long loginTime;
    private static boolean canRender;

    public static void resetRenderTimeout()
    {
        canRender = false;
        loginTime = System.currentTimeMillis();
    }

    public static void renderOverlays(float tickDelta)
    {
        Entity entity = GameUtils.getCameraEntity();

        if (entity == null)
        {
            return;
        }

        double x = EntityWrap.getX(entity);
        double y = EntityWrap.getY(entity);
        double z = EntityWrap.getZ(entity);

        if (canRender == false)
        {
            // Don't render before the player has been placed in the actual proper position,
            // otherwise some of the renderers mess up.
            // The magic 8.5, 65, 8.5 comes from the WorldClient constructor
            if (System.currentTimeMillis() - loginTime >= 5000 ||
                x != 8.5 ||
                y != 65 ||
                z != 8.5)
            {
                canRender = true;
            }
            else
            {
                return;
            }
        }

        double dx = EntityWrap.lerpX(entity, tickDelta);
        double dy = EntityWrap.lerpY(entity, tickDelta);
        double dz = EntityWrap.lerpZ(entity, tickDelta);

        if (RendererToggle.CHUNK_UNLOAD_BUCKET.isRendererEnabled())
        {
            renderChunkUnloadBuckets(entity, dx, dy, dz, Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y.getDoubleValue());
        }

        if (RendererToggle.BEACON_RANGE.isRendererEnabled())
        {
            renderBeaconBoxForPlayerIfHoldingItem(GameUtils.getClientPlayer(), dx, dy, dz);
        }

        if (RendererToggle.SPAWNER_POSITIONS.isRendererEnabled())
        {
            RenderContainer.SPAWNER_RENDERER.renderPositionText(dx, dy, dz);
        }

        if (RendererToggle.WATER_FALLS.isRendererEnabled())
        {
            RenderContainer.WATER_FALL_RENDERER.renderPositionText(dx, dy, dz);
        }
    }

    private static void renderChunkUnloadBuckets(Entity entity, double dx, double dy, double dz, double chunkOverlayY)
    {
        final int centerX = EntityWrap.getChunkX(entity);
        final int centerZ = EntityWrap.getChunkZ(entity);
        final float y = (float) chunkOverlayY;
        final float scale = Configs.Generic.CHUNK_UNLOAD_BUCKET_FONT_SCALE.getFloatValue();
        int r = Configs.Generic.CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS.getIntegerValue();

        if (r == -1)
        {
            r = GameUtils.getRenderDistanceChunks();
        }

        for (int xOff = -r; xOff <= r; xOff++)
        {
            for (int zOff = -r; zOff <= r; zOff++)
            {
                int cx = centerX + xOff;
                int cz = centerZ + zOff;
                int bucket = MiscUtils.getChunkUnloadBucket(cx, cz);
                String str = String.valueOf(bucket);
                TextRenderUtils.renderTextPlate(Collections.singletonList(str), (cx << 4) + 8.5d - dx, y - dy, (cz << 4) + 8.5D - dz, scale);
            }
        }
    }

    public static void renderBeaconBoxForPlayerIfHoldingItem(EntityPlayer player, double dx, double dy, double dz)
    {
        Item item = player.getHeldItemMainhand().getItem();

        if (item instanceof ItemBlock && ((ItemBlock) item).getBlock() == Blocks.BEACON)
        {
            renderBeaconBoxForPlayer(player, dx, dy, dz);
            return;
        }

        item = player.getHeldItemOffhand().getItem();

        if (item instanceof ItemBlock && ((ItemBlock) item).getBlock() == Blocks.BEACON)
        {
            renderBeaconBoxForPlayer(player, dx, dy, dz);
        }
    }

    private static void renderBeaconBoxForPlayer(EntityPlayer player, double dx, double dy, double dz)
    {
        double x = Math.floor(EntityWrap.getX(player)) - dx;
        double y = Math.floor(EntityWrap.getY(player)) - dy;
        double z = Math.floor(EntityWrap.getZ(player)) - dz;
        // Use the slot number as the level if sneaking
        int level = player.isSneaking() ? Math.min(4, player.inventory.currentItem + 1) : 4;
        double range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = y + 4;
        double maxZ = z + range + 1;
        Color4f color = OverlayRendererBeaconRange.getColorForLevel(level);

        GlStateManager.disableTexture2D();
        GlStateManager.enableAlpha();
        GlStateManager.alphaFunc(GL11.GL_GREATER, 0.01F);
        GlStateManager.disableCull();
        GlStateManager.disableLighting();
        GlStateManager.enableDepth();
        GlStateManager.depthMask(false);
        GlStateManager.doPolygonOffset(-3f, -3f);
        GlStateManager.enablePolygonOffset();
        GlStateManager.glLineWidth(1f);
        RenderUtils.setupBlend();
        RenderUtils.color(1f, 1f, 1f, 1f);

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder buffer = tessellator.getBuffer();
        buffer.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_COLOR);

        ShapeRenderUtils.renderBoxSideQuads(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(0.3f), buffer);

        tessellator.draw();
        buffer.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION_COLOR);

        ShapeRenderUtils.renderBoxEdgeLines(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(1f), buffer);

        tessellator.draw();

        GlStateManager.doPolygonOffset(0f, 0f);
        GlStateManager.disablePolygonOffset();
        GlStateManager.enableCull();
        GlStateManager.enableTexture2D();
        GlStateManager.disableBlend();
    }
}
