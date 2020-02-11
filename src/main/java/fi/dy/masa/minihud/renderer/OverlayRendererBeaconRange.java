package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import org.lwjgl.opengl.GL11;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBlock;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.tileentity.TileEntityBeacon;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

public class OverlayRendererBeaconRange extends OverlayRendererBase
{
    private static final Set<BlockPos> BEACON_POSITIONS = new HashSet<>();
    private static final Set<ChunkPos> BEACON_CHUNKS = new HashSet<>();

    private static boolean needsUpdate;

    public static void clear()
    {
        synchronized (BEACON_POSITIONS)
        {
            BEACON_CHUNKS.clear();
            BEACON_POSITIONS.clear();
        }
    }

    public static void setNeedsUpdate()
    {
        if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() == false)
        {
            clear();
        }

        needsUpdate = true;
    }

    public static void checkNeedsUpdate(BlockPos pos, IBlockState state)
    {
        synchronized (BEACON_POSITIONS)
        {
            if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() &&
                (state.getBlock() == Blocks.BEACON || BEACON_POSITIONS.contains(pos)))
            {
                setNeedsUpdate();
            }
        }
    }

    public static void checkNeedsUpdate(ChunkPos chunkPos)
    {
        synchronized (BEACON_POSITIONS)
        {
            if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() &&
                BEACON_CHUNKS.contains(chunkPos))
            {
                setNeedsUpdate();
            }
        }
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        if (needsUpdate || this.lastUpdatePos == null)
        {
            return true;
        }

        return false;
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        clear();

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        synchronized (BEACON_POSITIONS)
        {
            this.renderBeaconRanges(entity.getEntityWorld(), BUFFER_1, BUFFER_2);
        }

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();
        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        needsUpdate = false;
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }

    protected static Color4f getColorForLevel(int level)
    {
        switch (level)
        {
            case 1: return Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.getColor();
            case 2: return Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.getColor();
            case 3: return Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.getColor();
            default: return Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.getColor();
        }
    }

    protected void renderBeaconRanges(World world, BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        for (TileEntity be : world.loadedTileEntityList)
        {
            if (be instanceof TileEntityBeacon)
            {
                BlockPos pos = be.getPos();
                int level = ((TileEntityBeacon) be).getLevels();

                if (level >= 1 && level <= 4)
                {
                    this.renderBeaconBox(world, pos, level, getColorForLevel(level), bufferQuads, bufferLines);
                }
            }
        }
    }

    protected void renderBeaconBox(World world, BlockPos pos, int level, Color4f color, BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        int range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = this.getMaxHeight(world, pos, range);
        double maxZ = z + range + 1;

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color, bufferQuads);
        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(1f), bufferLines);

        BEACON_POSITIONS.add(pos);
        BEACON_CHUNKS.add(new ChunkPos(pos.getX() >> 4, pos.getZ() >> 4));
    }

    protected int getMaxHeight(World world, BlockPos pos, int range)
    {
        final int minX = pos.getX() - range;
        final int minZ = pos.getZ() - range;
        final int maxX = pos.getX() + range;
        final int maxZ = pos.getZ() + range;

        final int minCX = minX >> 4;
        final int minCZ = minZ >> 4;
        final int maxCX = maxX >> 4;
        final int maxCZ = maxZ >> 4;
        int maxY = 0;

        for (int cz = minCZ; cz <= maxCZ; ++cz)
        {
            for (int cx = minCX; cx <= maxCX; ++cx)
            {
                final int xMin = Math.max(minX,  cx << 4      );
                final int zMin = Math.max(minZ,  cz << 4      );
                final int xMax = Math.min(maxX, (cx << 4) + 15);
                final int zMax = Math.min(maxZ, (cz << 4) + 15);
                Chunk chunk = world.getChunk(cx, cz);

                for (int z = zMin; z <= zMax; ++z)
                {
                    for (int x = xMin; x <= xMax; ++x)
                    {
                        int height = chunk.getHeightValue(x & 0xF, z & 0xF);

                        if (height > maxY)
                        {
                            maxY = height;
                        }
                    }
                }
            }
        }

        return maxY + 4;
    }

    public static void renderBeaconBoxForPlayerIfHoldingItem(EntityPlayer player, double dx, double dy, double dz, float partialTicks)
    {
        Item item = player.getHeldItemMainhand().getItem();

        if (item instanceof ItemBlock && ((ItemBlock) item).getBlock() == Blocks.BEACON)
        {
            renderBeaconBoxForPlayer(player, dx, dy, dz, partialTicks);
            return;
        }

        item = player.getHeldItemOffhand().getItem();

        if (item instanceof ItemBlock && ((ItemBlock) item).getBlock() == Blocks.BEACON)
        {
            renderBeaconBoxForPlayer(player, dx, dy, dz, partialTicks);
            return;
        }
    }

    private static void renderBeaconBoxForPlayer(EntityPlayer player, double dx, double dy, double dz, float partialTicks)
    {
        double x = Math.floor(player.posX) - dx;
        double y = Math.floor(player.posY) - dy;
        double z = Math.floor(player.posZ) - dz;
        // Use the slot number as the level if sneaking
        int level = player.isSneaking() ? Math.min(4, player.inventory.currentItem + 1) : 4;
        double range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = y + 4;
        double maxZ = z + range + 1;
        Color4f color = getColorForLevel(level);

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
        fi.dy.masa.malilib.render.RenderUtils.setupBlend();
        fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder buffer = tessellator.getBuffer();
        buffer.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_COLOR);

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(0.3f), buffer);

        tessellator.draw();
        buffer.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION_COLOR);

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(1f), buffer);

        tessellator.draw();

        GlStateManager.doPolygonOffset(0f, 0f);
        GlStateManager.disablePolygonOffset();
        GlStateManager.enableCull();
        GlStateManager.enableTexture2D();
        GlStateManager.disableBlend();
    }
}
