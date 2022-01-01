package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import com.mojang.blaze3d.systems.RenderSystem;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.block.entity.BeaconBlockEntity;
import net.minecraft.block.entity.BlockEntity;
import net.minecraft.block.entity.BlockEntityType;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.GameRenderer;
import net.minecraft.client.render.Tessellator;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.item.BlockItem;
import net.minecraft.item.Item;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Matrix4f;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.chunk.BlockEntityTickInvoker;
import net.minecraft.world.chunk.WorldChunk;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.mixin.IMixinBeaconBlockEntity;
import fi.dy.masa.minihud.mixin.IMixinWorld;

public class OverlayRendererBeaconRange extends OverlayRendererBase
{
    private static final Set<BlockPos> BEACON_POSITIONS = new HashSet<>();
    private static final LongOpenHashSet BEACON_CHUNKS = new LongOpenHashSet();

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

    public static void checkNeedsUpdate(BlockPos pos, BlockState state)
    {
        synchronized (BEACON_POSITIONS)
        {
            if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() &&
                Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue() &&
                (state.getBlock() == Blocks.BEACON || BEACON_POSITIONS.contains(pos)))
            {
                setNeedsUpdate();
            }
        }
    }

    public static boolean checkNeedsUpdate(int chunkX, int chunkZ)
    {
        synchronized (BEACON_POSITIONS)
        {
            if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() &&
                Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue() &&
                BEACON_CHUNKS.contains(ChunkPos.toLong(chunkX, chunkZ)))
            {
                setNeedsUpdate();
            }
        }

        return needsUpdate;
    }

    public static boolean checkNeedsUpdate(List<NbtCompound> beList)
    {
        synchronized (BEACON_POSITIONS)
        {
            if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() &&
                Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue())
            {
                for (NbtCompound tag : beList)
                {
                    if (tag.getString("id").equals("minecraft:beacon"))
                    {
                        setNeedsUpdate();
                        break;
                    }
                }
            }
        }

        return needsUpdate;
    }

    private static void checkNeedsUpdate(BlockEntityType<?> type)
    {
        if (type == BlockEntityType.BEACON)
        {
            setNeedsUpdate();
        }
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        return needsUpdate || this.lastUpdatePos == null;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        clear();

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        synchronized (BEACON_POSITIONS)
        {
            this.renderBeaconRanges(entity.getEntityWorld(), cameraPos, BUFFER_1, BUFFER_2);
        }

        BUFFER_1.end();
        BUFFER_2.end();
        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        needsUpdate = false;
    }

    protected static Color4f getColorForLevel(int level)
    {
        switch (level)
        {
            case 1: return Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.getColor();
            case 2: return Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.getColor();
            case 3: return Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.getColor();
            default: return Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.getColor();
        }
    }

    protected void renderBeaconRanges(World world, Vec3d cameraPos, BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        for (BlockEntityTickInvoker ticker : ((IMixinWorld) world).minihud_getBlockEntityTickers())
        {
            BlockEntity be = world.getBlockEntity(ticker.getPos());

            if (be instanceof BeaconBlockEntity)
            {
                BlockPos pos = be.getPos();
                int level = ((IMixinBeaconBlockEntity) be).minihud_getLevel();

                if (level >= 1 && level <= 4)
                {
                    this.renderBeaconBox(world, pos, level, cameraPos, getColorForLevel(level), bufferQuads, bufferLines);
                }
            }
        }
    }

    protected void renderBeaconBox(World world, BlockPos pos, int level, Vec3d cameraPos,
                                   Color4f color, BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        double x = pos.getX() - cameraPos.x;
        double y = pos.getY() - cameraPos.y;
        double z = pos.getZ() - cameraPos.z;

        int range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = this.getMaxHeight(world, pos, range);
        double maxZ = z + range + 1;

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color, bufferQuads);
        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, Color4f.fromColor(color, 1f), bufferLines);

        BEACON_POSITIONS.add(pos);
        BEACON_CHUNKS.add(ChunkPos.toLong(pos.getX() >> 4, pos.getZ() >> 4));
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
                WorldChunk chunk = world.getChunk(cx, cz);
                int height = chunk.getHighestNonEmptySectionYOffset() + 15;

                if (height > maxY)
                {
                    maxY = height;
                }
            }
        }

        return maxY + 4;
    }

    public static void renderBeaconBoxForPlayerIfHoldingItem(Entity entity, MatrixStack matrixStack,
                                                             Matrix4f projMatrix, MinecraftClient mc)
    {
        Item item = mc.player.getMainHandStack().getItem();

        if (item instanceof BlockItem && ((BlockItem) item).getBlock() == Blocks.BEACON)
        {
            renderBeaconBoxForPlayer(entity, matrixStack, projMatrix, mc);
            return;
        }

        item = mc.player.getMainHandStack().getItem();

        if (item instanceof BlockItem && ((BlockItem) item).getBlock() == Blocks.BEACON)
        {
            renderBeaconBoxForPlayer(entity, matrixStack, projMatrix, mc);
        }
    }

    private static void renderBeaconBoxForPlayer(Entity entity, MatrixStack matrixStack,
                                                 Matrix4f projMatrix, MinecraftClient mc)
    {
        Vec3d cameraPos = mc.gameRenderer.getCamera().getPos();
        double x = Math.floor(entity.getX()) - cameraPos.x;
        double y = Math.floor(entity.getY()) - cameraPos.y;
        double z = Math.floor(entity.getZ()) - cameraPos.z;
        // Use the slot number as the level if sneaking
        int level = mc.player.isSneaking() ? Math.min(4, mc.player.getInventory().selectedSlot + 1) : 4;
        double range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = y + 4;
        double maxZ = z + range + 1;
        Color4f color = getColorForLevel(level);

        RenderSystem.disableTexture();
        RenderSystem.disableCull();
        RenderSystem.enableDepthTest();
        RenderSystem.depthMask(false);
        RenderSystem.polygonOffset(-3f, -3f);
        RenderSystem.enablePolygonOffset();
        fi.dy.masa.malilib.render.RenderUtils.setupBlend();
        fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder buffer = tessellator.getBuffer();

        RenderSystem.setShader(GameRenderer::getPositionColorShader);
        RenderSystem.applyModelViewMatrix();
        buffer.begin(VertexFormat.DrawMode.QUADS, VertexFormats.POSITION_COLOR);

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, Color4f.fromColor(color, 0.3f), buffer);

        tessellator.draw();

        buffer.begin(VertexFormat.DrawMode.DEBUG_LINES, VertexFormats.POSITION_COLOR);

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, Color4f.fromColor(color, 1f), buffer);

        tessellator.draw();

        RenderSystem.polygonOffset(0f, 0f);
        RenderSystem.disablePolygonOffset();
        RenderSystem.enableCull();
        RenderSystem.enableTexture();
        RenderSystem.disableBlend();
    }
}
