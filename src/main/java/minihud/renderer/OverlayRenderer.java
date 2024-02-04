package minihud.renderer;

import java.util.Collections;
import org.lwjgl.opengl.GL11;

import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBlock;

import malilib.render.RenderContext;
import malilib.render.ShapeRenderUtils;
import malilib.render.TextRenderUtils;
import malilib.render.buffer.VanillaWrappingVertexBuilder;
import malilib.render.buffer.VertexBuilder;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameWrap;
import malilib.util.game.wrap.RenderWrap;
import malilib.util.inventory.InventoryUtils;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.util.MiscUtils;

public class OverlayRenderer
{
    private static long loginTime;
    private static boolean canRender;

    public static void resetRenderTimeout()
    {
        canRender = false;
        loginTime = System.nanoTime();
    }

    public static void renderOverlays(RenderContext ctx, float tickDelta)
    {
        Entity entity = GameWrap.getCameraEntity();

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
            if (System.nanoTime() - loginTime >= 5000000000L ||
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
            double overlayY = Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y.getDoubleValue();
            renderChunkUnloadBuckets(entity, dx, dy, dz, overlayY, ctx);
        }

        if (RendererToggle.BEACON_RANGE.isRendererEnabled())
        {
            renderBeaconBoxForPlayerIfHoldingItem(GameWrap.getClientPlayer(), dx, dy, dz);
        }

        if (RendererToggle.SPAWNER_POSITIONS.isRendererEnabled())
        {
            RenderContainer.SPAWNER_RENDERER.renderPositionText(dx, dy, dz, ctx);
        }

        if (RendererToggle.WATER_FALLS.isRendererEnabled())
        {
            RenderContainer.WATER_FALL_RENDERER.renderPositionText(dx, dy, dz, ctx);
        }
    }

    private static void renderChunkUnloadBuckets(Entity entity, double dx, double dy, double dz,
                                                 double chunkOverlayY, RenderContext ctx)
    {
        final int centerX = EntityWrap.getChunkX(entity);
        final int centerZ = EntityWrap.getChunkZ(entity);
        final float y = (float) chunkOverlayY;
        final float scale = Configs.Generic.CHUNK_UNLOAD_BUCKET_FONT_SCALE.getFloatValue();
        int r = Configs.Generic.CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS.getIntegerValue();

        if (r == -1)
        {
            r = GameWrap.getRenderDistanceChunks();
        }

        for (int xOff = -r; xOff <= r; xOff++)
        {
            for (int zOff = -r; zOff <= r; zOff++)
            {
                int cx = centerX + xOff;
                int cz = centerZ + zOff;
                int bucket = MiscUtils.getChunkUnloadBucket(cx, cz);
                String str = String.valueOf(bucket);
                TextRenderUtils.renderTextPlate(Collections.singletonList(str),
                                                (cx << 4) + 8.5d - dx, y - dy, (cz << 4) + 8.5D - dz, scale, ctx);
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
        int level = player.isSneaking() ? Math.min(4, InventoryUtils.getSelectedHotbarSlot() + 1) : 4;
        double range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = y + 4;
        double maxZ = z + range + 1;
        Color4f color = OverlayRendererBeaconRange.getColorForLevel(level);

        RenderWrap.enableAlpha();
        RenderWrap.alphaFunc(GL11.GL_GREATER, 0.01F);
        RenderWrap.disableCull();
        RenderWrap.disableLighting();
        RenderWrap.enableDepthTest();
        RenderWrap.depthMask(false);
        RenderWrap.polygonOffset(-3f, -3f);
        RenderWrap.enablePolygonOffset();
        RenderWrap.lineWidth(1f);
        RenderWrap.color(1f, 1f, 1f, 1f);

        VertexBuilder quadBuilder = VanillaWrappingVertexBuilder.coloredQuads();
        ShapeRenderUtils.renderBoxSideQuads(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(0.3f), quadBuilder);
        quadBuilder.draw();

        VertexBuilder lineBuilder = VanillaWrappingVertexBuilder.coloredLines();
        ShapeRenderUtils.renderBoxEdgeLines(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(1f), lineBuilder);
        lineBuilder.draw();

        RenderWrap.polygonOffset(0f, 0f);
        RenderWrap.disablePolygonOffset();
        RenderWrap.enableCull();
    }
}
