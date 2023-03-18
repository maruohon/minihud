package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.IntFunction;
import javax.annotation.Nullable;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
import it.unimi.dsi.fastutil.objects.Object2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.registry.Registry;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.World;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.biome.source.BiomeAccess;
import net.minecraft.world.chunk.ChunkStatus;
import net.minecraft.world.chunk.WorldChunk;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.SubChunkPos;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;

public class OverlayRendererBiomeBorders extends OverlayRendererBase
{
    public static final OverlayRendererBiomeBorders INSTANCE = new OverlayRendererBiomeBorders();

    private final Object2ObjectOpenHashMap<SubChunkPos, List<ColoredQuad>> quads = new Object2ObjectOpenHashMap<>();
    private final Object2IntOpenHashMap<Biome> biomeMapping = new Object2IntOpenHashMap<>();
    private final Int2ObjectOpenHashMap<Color4f> biomeColorsMap = new Int2ObjectOpenHashMap<>();
    private final ObjectOpenHashSet<SubChunkPos> scheduledChunks = new ObjectOpenHashSet<>();
    private final Color4f fixedColor = Color4f.fromColor(0x30F030, 0.25f);
    private Color4f[] biomeColorsArray = new Color4f[0];
    private IntFunction<Color4f> colorRetriever = (id) -> this.biomeColorsArray[id];
    private Vec3d cameraPosition = Vec3d.ZERO;
    //private long lastUpdateTime = System.nanoTime();
    private boolean needsUpdate;
    private boolean needsRenderUpdate;

    private OverlayRendererBiomeBorders()
    {
        this.useCulling = true;
    }

    public void setNeedsUpdate()
    {
        if (RendererToggle.OVERLAY_BIOME_BORDER.getBooleanValue())
        {
            // All the quads need to have the same relative camera offset, so
            // we use an internal position that is only updated when all the quads are cleared
            this.cameraPosition = MinecraftClient.getInstance().gameRenderer.getCamera().getPos();

            this.needsUpdate = true;
            this.clear(); // FIXME debug?
        }
    }

    public void clear()
    {
        synchronized (this.quads)
        {
            MiniHUD.printDebug("Clearing Biome Border Overlay data...");

            this.quads.clear();
            this.scheduledChunks.clear();
            this.biomeMapping.clear();

            // All the quads need to have the same relative camera offset, so
            // we use an internal position that is only updated when all the quads are cleared
            this.cameraPosition = MinecraftClient.getInstance().gameRenderer.getCamera().getPos();
        }
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_BIOME_BORDER.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity cameraEntity, MinecraftClient mc)
    {
        int updateDistance = 16;

        return this.needsUpdate ||
               this.needsRenderUpdate ||
               this.lastUpdatePos == null ||
                Math.abs(cameraEntity.getX() - this.lastUpdatePos.getX()) > updateDistance ||
                Math.abs(cameraEntity.getY() - this.lastUpdatePos.getY()) > updateDistance ||
                Math.abs(cameraEntity.getZ() - this.lastUpdatePos.getZ()) > updateDistance;
    }

    @Override
    public void update(Vec3d cameraPos, Entity cameraEntity, MinecraftClient mc)
    {
        List<SubChunkPos> chunks = this.getSubChunksWithinRange(cameraEntity, mc);
        BlockPos cameraBlockPos = BlockPos.ofFloored(cameraPos);
        this.scheduleTasksForMissingChunks(chunks, cameraBlockPos, mc.world);

        List<ColoredQuad> quads = this.getQuadsToRender(chunks);
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        this.renderQuads(quads, BUFFER_1, BUFFER_2, this.cameraPosition);

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        // All the quads need to have the same relative camera offset, so
        // we use an internal position that is only updated when all the quads are cleared
        this.setUpdatePosition(this.cameraPosition);
        this.needsUpdate = false;
        this.needsRenderUpdate = false;
    }

    protected void renderQuads(List<ColoredQuad> quads, BufferBuilder quadBuffer,
                               BufferBuilder lineBuffer, Vec3d cameraPos)
    {
        double inset = 0.0001;

        //long pre = System.nanoTime();
        for (ColoredQuad quad : quads)
        {
            Color4f color = this.getColor(quad.biomeId);
            RenderUtils.renderInsetQuad(quad.start, quad.width, quad.height, quad.side, inset, color, cameraPos, quadBuffer);
            RenderUtils.renderBiomeBorderLines(quad.start, quad.width, quad.height, quad.side, inset, color, cameraPos, lineBuffer);
        }
        //long post = System.nanoTime(); System.out.printf("renderQuads: %.6fs\n", ((double) post - (double) pre) / 1000000000D);
    }

    protected List<SubChunkPos> getSubChunksWithinRange(Entity cameraEntity, MinecraftClient mc)
    {
        //long pre = System.nanoTime();
        World world = mc.world;
        int viewDistance = Math.min(Configs.Generic.BIOME_OVERLAY_RANGE.getIntegerValue(), mc.options.getViewDistance().getValue());
        int viewDistanceVertical = Math.min(Configs.Generic.BIOME_OVERLAY_RANGE_VERTICAL.getIntegerValue(), mc.options.getViewDistance().getValue());
        int chunkX = MathHelper.floor(cameraEntity.getX()) >> 4;
        int chunkY = MathHelper.floor(cameraEntity.getY()) >> 4;
        int chunkZ = MathHelper.floor(cameraEntity.getZ()) >> 4;
        int minCY = Math.max(world.getBottomY() >> 4, chunkY - viewDistanceVertical);
        int maxCY = Math.min((world.getTopY() - 1) >> 4, chunkY + viewDistanceVertical);
        List<SubChunkPos> chunks = new ArrayList<>();

        for (int cz = chunkZ - viewDistance; cz <= chunkZ + viewDistance; ++cz)
        {
            for (int cx = chunkX - viewDistance; cx <= chunkX + viewDistance; ++cx)
            {
                for (int cy = minCY; cy <= maxCY; ++cy)
                {
                    chunks.add(new SubChunkPos(cx, cy, cz));
                }
            }
        }
        //long post = System.nanoTime(); System.out.printf("getSubChunksWithinRange: %.6fs, sub-chunks: %d\n", ((double) post - (double) pre) / 1000000000D, chunks.size());

        return chunks;
    }

    protected List<ColoredQuad> getQuadsToRender(List<SubChunkPos> chunks)
    {
        //long pre = System.nanoTime();
        List<ColoredQuad> quads = new ArrayList<>();

        synchronized (this.quads)
        {
            for (SubChunkPos pos : chunks)
            {
                List<ColoredQuad> tmp = this.quads.get(pos);

                if (tmp != null)
                {
                    quads.addAll(tmp);
                }
            }
        }
        //long post = System.nanoTime(); System.out.printf("getQuadsToRender: %.6fs, quads: %d\n", ((double) post - (double) pre) / 1000000000D, quads.size());

        return quads;
    }

    protected void scheduleTasksForMissingChunks(List<SubChunkPos> chunks, BlockPos cameraBlockPos, World world)
    {
        //long pre = System.nanoTime(); int count = 0;
        synchronized (this.quads)
        {
            if (this.biomeMapping.isEmpty())
            {
                this.createBiomeMapping(world);
            }

            if (this.quads.size() > 32000)
            {
                this.quads.clear();
            }

            chunks = new ArrayList<>(chunks);
            chunks.removeAll(this.scheduledChunks);
            chunks.removeAll(this.quads.keySet());
        }

        for (SubChunkPos pos : chunks)
        {
            WorldChunk chunk = (WorldChunk) world.getChunk(pos.getX(), pos.getZ(), ChunkStatus.FULL, false);

            if (chunk != null)
            {
                Runnable task = () -> {
                    List<ColoredQuad> quadsOut = new ArrayList<>();
                    readBiomesAndFindEdges(chunk, pos, quadsOut, this.biomeMapping);

                    synchronized (this.quads)
                    {
                        this.quads.put(pos, quadsOut);
                        this.scheduledChunks.remove(pos);
                        this.needsRenderUpdate = true;
                    }
                };

                //++count;
                synchronized (this.quads)
                {
                    this.scheduledChunks.add(pos);
                }

                DataStorage.INSTANCE.addTask(task, chunk.getPos(), cameraBlockPos);
            }
        }
        //long post = System.nanoTime(); System.out.printf("scheduleTasksForMissingChunks: %.6fs, scheduled: %d\n", ((double) post - (double) pre) / 1000000000D, count);

        this.needsUpdate = false;
        //this.lastUpdateTime = System.nanoTime();
    }

    private static int readBiomesAndFindEdges(WorldChunk chunk,
                                              SubChunkPos subChunkPos,
                                              List<ColoredQuad> quadsOut,
                                              Object2IntOpenHashMap<Biome> biomeMapping)
    {
        //long pre = System.nanoTime();
        final int startX = (subChunkPos.getX() << 4) - 1;
        final int startY = (subChunkPos.getY() << 4) - 1;
        final int startZ = (subChunkPos.getZ() << 4) - 1;
        Biome[][][] biomes = new Biome[18][18][18];
        BiomeSource biomeSource = getBiomeSourceForChunk(chunk);

        // Read the biomes for the given chunk section, plus a one block shell around it
        for (int y = 0; y < 18; ++y)
        {
            for (int z = 0; z < 18; ++z)
            {
                for (int x = 0; x < 18; ++x)
                {
                    biomes[x][y][z] = biomeSource.getBiome(startX + x, startY + y, startZ + z);
                }
            }
        }
        //long post = System.nanoTime(); System.out.printf("readBiomesAndFindEdges, reading biome data: %.6fs\n", ((double) post - (double) pre) / 1000000000D);

        return buildQuadsFromBiomeData(startX, startY, startZ, biomes, quadsOut, biomeMapping);
    }

    private static int buildQuadsFromBiomeData(int minX, int minY, int minZ,
                                               Biome[][][] biomes,
                                               List<ColoredQuad> quadsOut,
                                               Object2IntOpenHashMap<Biome> biomeMapping)
    {
        //long pre = System.nanoTime();
        ArrayList<EdgeStrip> stripList = new ArrayList<>();
        EdgeStrip[][][][] strips = new EdgeStrip[16][16][16][6];
        Direction[] sides = new Direction[] { Direction.NORTH, Direction.SOUTH, Direction.DOWN, Direction.UP };
        int[] startPos = new int[6];
        BlockPos minCorner = new BlockPos(minX, minY, minZ);
        Direction.Axis axis = Direction.Axis.X;

        // The biomes array is 18x18x18, having one block around the sub-chunk
        for (int y = 1; y < 17; ++y)
        {
            for (int z = 1; z < 17; ++z)
            {
                Arrays.fill(startPos, -1);
                Biome lastBiome = null;

                for (int x = 1; x < 17; ++x)
                {
                    Biome biome = biomes[x][y][z];
                    buildStrips(x, y, z, axis, minCorner, biome, lastBiome, startPos, biomes, sides, strips, stripList, biomeMapping);
                    lastBiome = biome;
                }

                endStrips(17, y, z, axis, minCorner, lastBiome, startPos, sides, strips, stripList, biomeMapping);
            }
        }

        sides = new Direction[] { Direction.WEST, Direction.EAST };
        axis = Direction.Axis.Z;

        for (int y = 1; y < 17; ++y)
        {
            for (int x = 1; x < 17; ++x)
            {
                Arrays.fill(startPos, -1);
                Biome lastBiome = null;

                for (int z = 1; z < 17; ++z)
                {
                    Biome biome = biomes[x][y][z];
                    buildStrips(x, y, z, axis, minCorner, biome, lastBiome, startPos, biomes, sides, strips, stripList, biomeMapping);
                    lastBiome = biome;
                }

                endStrips(x, y, 17, axis, minCorner, lastBiome, startPos, sides, strips, stripList, biomeMapping);
            }
        }
        //long post = System.nanoTime(); System.out.printf("buildQuadsFromBiomeData, strips: %.6fs - count: %d\n", ((double) post - (double) pre) / 1000000000D, stripList.size());

        return buildStripsToQuads(strips, stripList, quadsOut);
    }

    private static void buildStrips(int x, int y, int z, Direction.Axis axis, BlockPos minCorner,
                                    Biome biome, @Nullable Biome lastBiome, int[] startPos,
                                    Biome[][][] biomes, Direction[] sides, EdgeStrip[][][][] strips,
                                    ArrayList<EdgeStrip> stripList, Object2IntOpenHashMap<Biome> biomeMapping)
    {
        int biomeId = -1;
        int pos = axis == Direction.Axis.X ? x : z;
        // Note: The min coordinates are adjusted by -1 already
        int stripX = minCorner.getX() + x;
        int stripY = minCorner.getY() + y;
        int stripZ = minCorner.getZ() + z;

        for (Direction side : sides)
        {
            Biome adjBiome = biomes[x + side.getOffsetX()][y + side.getOffsetY()][z + side.getOffsetZ()];
            int sideIndex = side.getId();
            int stripStart = startPos[sideIndex];
            //if (stripX == 100 && stripY == 65 && stripZ < -80 && side == Direction.EAST)
            //System.out.printf("biome @ [%d, %d, %d]: %d, adj @ %s = %d, ss: %d\n", minCorner.getX() + x, minCorner.getY() + y, minCorner.getZ() + z, biomeMapping.getInt(biome), side, biomeMapping.getInt(adjBiome), stripStart);

            // Either: Biome changed in the current column,
            // or there was a started strip for the side,
            // but the adjacent biome now became the same as this column.
            // So end and add the strip.
            if (stripStart >= 0 && lastBiome != null && (biome != lastBiome || biome == adjBiome))
            {
                if (biomeId == -1)
                {
                    biomeId = biomeMapping.getInt(lastBiome);
                }

                if (axis == Direction.Axis.X)
                {
                    stripX = minCorner.getX() + stripStart;
                }
                else if (axis == Direction.Axis.Z)
                {
                    stripZ = minCorner.getZ() + stripStart;
                }
                else if (axis == Direction.Axis.Y)
                {
                    stripY = minCorner.getY() + stripStart;
                }
                //if (stripX == 100 && stripY == 65 && stripZ < -80 && side == Direction.EAST)
                //System.out.printf("adding strip @ [%d, %d, %d]: side: %s, len: %d, pos: %d, ss: %d\n", stripX, stripY, stripZ, side, pos - stripStart, pos, stripStart);

                long packedPos = getPackedCoordinate(stripX, stripY, stripZ);
                EdgeStrip strip = new EdgeStrip(packedPos, side, pos - stripStart, biomeId);

                strips[stripX & 0xF][stripY & 0xF][stripZ & 0xF][sideIndex] = strip;
                stripList.add(strip);
                stripStart = -1;
                startPos[sideIndex] = -1;
            }

            // No strip started yet
            if (biome != adjBiome && stripStart < 0)
            {
                //if (stripX == 100 && stripY == 65 && stripZ < -80 && side == Direction.EAST)
                //System.out.printf("starting strip @ [%d, %d, %d] adj @ %s = %d, ss: %d\n", minCorner.getX() + x, minCorner.getY() + y, minCorner.getZ() + z, side, biomeMapping.getInt(adjBiome), pos);
                startPos[sideIndex] = pos;
            }
        }
    }

    private static void endStrips(int x, int y, int z, Direction.Axis axis, BlockPos minCorner,
                                  Biome lastBiome, int[] startPos,
                                  Direction[] sides, EdgeStrip[][][][] strips, ArrayList<EdgeStrip> stripList,
                                  Object2IntOpenHashMap<Biome> biomeMapping)
    {
        int biomeId = -1;
        // Note: The min coordinates are adjusted by -1 already
        int stripX = minCorner.getX() + x;
        int stripY = minCorner.getY() + y;
        int stripZ = minCorner.getZ() + z;

        for (Direction side : sides)
        {
            int sideIndex = side.getId();
            int stripStart = startPos[sideIndex];

            // There was a started strip for the side
            if (stripStart >= 0)
            {
                if (biomeId == -1)
                {
                    biomeId = biomeMapping.getInt(lastBiome);
                }

                if (axis == Direction.Axis.X)
                {
                    stripX = minCorner.getX() + stripStart;
                }
                else if (axis == Direction.Axis.Z)
                {
                    stripZ = minCorner.getZ() + stripStart;
                }
                else if (axis == Direction.Axis.Y)
                {
                    stripY = minCorner.getY() + stripStart;
                }

                //if (stripX == 100 && stripY == 65 && stripZ < -80 && side == Direction.EAST)
                //System.out.printf("endStrips adding strip @ [%d, %d, %d]: side: %s, len: %d\n", stripX, stripY, stripZ, side, 16 - stripStart + 1);
                long packedPos = getPackedCoordinate(stripX, stripY, stripZ);
                EdgeStrip strip = new EdgeStrip(packedPos, side, 16 - stripStart + 1, biomeId);

                strips[stripX & 0xF][stripY & 0xF][stripZ & 0xF][sideIndex] = strip;
                stripList.add(strip);
                startPos[sideIndex] = -1;
            }
        }
    }

    private static int buildStripsToQuads(EdgeStrip[][][][] strips,
                                          ArrayList<EdgeStrip> stripList,
                                          List<ColoredQuad> quadsOut)
    {
        //long pre = System.nanoTime();
        byte[][][] handled = new byte[16][16][16];

        for (EdgeStrip strip : stripList)
        {
            Direction side = strip.side;
            int sideIndex = side.getId();
            final long startPos = strip.startPosLong;
            int x = unpackX(startPos) & 0xF;
            int y = unpackY(startPos) & 0xF;
            int z = unpackZ(startPos) & 0xF;

            if ((handled[x][y][z] & (1 << sideIndex)) != 0)
            {
                continue;
            }

            Direction scanDir = side.getAxis() != Direction.Axis.Y ? Direction.UP : Direction.SOUTH;
            final int length = strip.length;
            final int biomeId = strip.biomeId;
            final int limit = 16 - (side.getAxis() != Direction.Axis.Y ? y : z);
            int count = 1;

            handled[x][y][z] |= (1 << sideIndex);

            for (int i = 1; i < limit; ++i)
            {
                int tmpX = x + scanDir.getOffsetX() * i;
                int tmpY = y + scanDir.getOffsetY() * i;
                int tmpZ = z + scanDir.getOffsetZ() * i;

                EdgeStrip nextStrip = strips[tmpX][tmpY][tmpZ][sideIndex];

                if (nextStrip == null || nextStrip.length != length || nextStrip.biomeId != biomeId)
                {
                    break;
                }

                handled[tmpX][tmpY][tmpZ] |= (1 << sideIndex);
                ++count;
            }

            quadsOut.add(makeColoredQuad(startPos, length, count, side, strip.biomeId));
        }
        //long post = System.nanoTime(); System.out.printf("buildStripsToQuads: %.6fs, quads: %d\n", ((double) post - (double) pre) / 1000000000D, quadsOut.size());

        return  quadsOut.size();
    }

    protected static ColoredQuad makeColoredQuad(long startPos, int stripLength, int stripCount,
                                                 Direction side, int biomeId)
    {
        int x1 = unpackX(startPos);
        int y1 = unpackY(startPos);
        int z1 = unpackZ(startPos);

        return new ColoredQuad(new Vec3i(x1, y1, z1), stripLength, stripCount, side, biomeId);
    }

    protected static int unpackX(long packedCoordinate)
    {
        return (int) ((packedCoordinate << 40L) >> 40L);
    }

    protected static int unpackY(long packedCoordinate)
    {
        return (int) (packedCoordinate >> 48L);
    }

    protected static int unpackZ(long packedCoordinate)
    {
        return (int) ((packedCoordinate << 16L) >> 40L);
    }

    protected static long getPackedCoordinate(int x, int y, int z)
    {
        return (((long) y & 0xFFFF) << 48L) | (((long) z & 0xFFFFFF) << 24L) | ((long) x & 0xFFFFFF);
    }

    protected static long getPackedCoordinate(BlockPos.Mutable pos)
    {
        return getPackedCoordinate(pos.getX(), pos.getY(), pos.getZ());
    }

    private void createBiomeMapping(World world)
    {
        this.biomeMapping.clear();
        this.biomeColorsMap.clear();
        this.biomeColorsArray = new Color4f[0];

        if (Configs.Generic.BIOME_OVERLAY_SINGLE_COLOR.getBooleanValue())
        {
            this.colorRetriever = (id) -> this.fixedColor;
        }
        else
        {
            final Registry<Biome> registry = world.getRegistryManager().get(RegistryKeys.BIOME);
            int count = 0;
            int maxId = 0;

            for (Biome biome : registry)
            {
                int id = registry.getRawId(biome);
                this.biomeMapping.put(biome, id);
                ++count;

                if (id > maxId)
                {
                    maxId = id;
                }
            }

            int hueInc = 360 / count;
            int hue = 0;

            if (maxId <= 65535)
            {
                this.biomeColorsArray = new Color4f[maxId + 1];
                this.colorRetriever = (id) -> this.biomeColorsArray[id];

                for (int id : this.biomeMapping.values())
                {
                    this.biomeColorsArray[id] = Color4f.fromColor(getColorFromHue(hue), 0.25f);
                    hue += hueInc;
                }
            }
            else
            {
                this.colorRetriever = this.biomeColorsMap::get;

                for (int id : this.biomeMapping.values())
                {
                    this.biomeColorsMap.put(id, Color4f.fromColor(getColorFromHue(hue), 0.25f));
                    hue += hueInc;
                }
            }

            this.addCustomColorMappings(registry);
        }
    }

    private void addCustomColorMappings(Registry<Biome> registry)
    {
        ColorRegistry reg;

        if (this.biomeColorsArray.length > 0)
        {
            reg = (biomeId, color) -> this.assignColor(biomeId, color, (id, c) -> this.biomeColorsArray[id] = c, registry);
        }
        else
        {
            reg = (biomeId, color) -> this.assignColor(biomeId, color, this.biomeColorsMap::put, registry);
        }

        reg.set("minecraft:badlands",                         0xFFC040);
        reg.set("minecraft:badlands_plateau",                 0xFFC040);
        reg.set("minecraft:bamboo_jungle",                    0x50FF10);
        reg.set("minecraft:bamboo_jungle_hills",              0x50FF10);
        reg.set("minecraft:basalt_deltas",                    0x888888);
        reg.set("minecraft:beach",                            0xF2EFC2);
        reg.set("minecraft:birch_forest",                     0x45D69B);
        reg.set("minecraft:birch_forest_hills",               0x45D69B);
        reg.set("minecraft:cold_ocean",                       0x0709EB);
        reg.set("minecraft:crimson_forest",                   0xFF0713);
        reg.set("minecraft:dark_forest",                      0x009301);
        reg.set("minecraft:dark_forest_hills",                0x6DA77C);
        reg.set("minecraft:deep_cold_ocean",                  0x261FB4);
        reg.set("minecraft:deep_frozen_ocean",                0x1F69B4);
        reg.set("minecraft:deep_lukewarm_ocean",              0x1FB3B4);
        reg.set("minecraft:deep_ocean",                       0x1D41E4);
        reg.set("minecraft:deep_warm_ocean",                  0x1DC5E4);
        reg.set("minecraft:desert",                           0xF2EE01);
        reg.set("minecraft:desert_hills",                     0xF2B801);
        reg.set("minecraft:desert_lakes",                     0xC0F201);
        reg.set("minecraft:dripstone_caves",                  0xB29268);
        reg.set("minecraft:end_barrens",                      0xD2CFB4);
        reg.set("minecraft:end_highlands",                    0xF0C136);
        reg.set("minecraft:end_midlands",                     0xE7F036);
        reg.set("minecraft:eroded_badlands",                  0xE278A9);
        reg.set("minecraft:flower_forest",                    0x71E78B);
        reg.set("minecraft:forest",                           0x21A13D);
        reg.set("minecraft:frozen_ocean",                     0x79C7EC);
        reg.set("minecraft:frozen_river",                     0xBDDEED);
        reg.set("minecraft:giant_spruce_taiga",               0xCB710B);
        reg.set("minecraft:giant_spruce_taiga_hills",         0xB74615);
        reg.set("minecraft:giant_tree_taiga",                 0xC29E45);
        reg.set("minecraft:giant_tree_taiga_hills",           0xBBAA7F);
        reg.set("minecraft:gravelly_mountains",               0x939088);
        reg.set("minecraft:ice_spikes",                       0x24FBFF);
        reg.set("minecraft:jungle",                           0x72EF5D);
        reg.set("minecraft:jungle_edge",                      0x3DE296);
        reg.set("minecraft:jungle_hills",                     0x26AD6F);
        reg.set("minecraft:lukewarm_ocean",                   0x22D9E6);
        reg.set("minecraft:lush_caves",                       0x4ACE47);
        reg.set("minecraft:modified_badlands_plateau",        0x7F7B21);
        reg.set("minecraft:modified_gravelly_mountains",      0x678FA4);
        reg.set("minecraft:modified_jungle",                  0x9FE961);
        reg.set("minecraft:modified_jungle_edge",             0x69BC23);
        reg.set("minecraft:modified_wooded_badlands_plateau", 0xBCB623);
        reg.set("minecraft:mountain_edge",                    0x818181);
        reg.set("minecraft:mountains",                        0xC5C5C5);
        reg.set("minecraft:mushroom_field_shore",             0xEC7F5E);
        reg.set("minecraft:mushroom_fields",                  0xED7109);
        reg.set("minecraft:nether_wastes",                    0x88AABC);
        reg.set("minecraft:ocean",                            0x0926ED);
        reg.set("minecraft:plains",                           0x1EA128);
        reg.set("minecraft:river",                            0x2D6EDA);
        reg.set("minecraft:savanna",                          0xBEBB61);
        reg.set("minecraft:savanna_plateau",                  0xEBE433);
        reg.set("minecraft:shattered_savanna",                0xEBB433);
        reg.set("minecraft:shattered_savanna_plateau",        0xB19D6F);
        reg.set("minecraft:small_end_islands",                0xC0CD03);
        reg.set("minecraft:snowy_beach",                      0xE0DD95);
        reg.set("minecraft:snowy_mountains",                  0xDDDDDD);
        reg.set("minecraft:snowy_taiga",                      0xC9EFC5);
        reg.set("minecraft:snowy_taiga_hills",                0x4FB043);
        reg.set("minecraft:snowy_taiga_mountains",            0xA1BBE9);
        reg.set("minecraft:snowy_tundra",                     0x62A15A);
        reg.set("minecraft:soul_sand_valley",                 0x8F510B);
        reg.set("minecraft:stone_shore",                      0xABABAB);
        reg.set("minecraft:sunflower_plains",                 0x93DF19);
        reg.set("minecraft:swamp",                            0x27960B);
        reg.set("minecraft:swamp_hills",                      0x89D267);
        reg.set("minecraft:taiga",                            0x026900);
        reg.set("minecraft:taiga_hills",                      0x405C3F);
        reg.set("minecraft:taiga_mountains",                  0x759875);
        reg.set("minecraft:tall_birch_forest",                0x56A45D);
        reg.set("minecraft:tall_birch_hills",                 0x7CE985);
        reg.set("minecraft:the_end",                          0xC7E030);
        reg.set("minecraft:the_void",                         0xF218F3);
        reg.set("minecraft:warm_ocean",                       0x189EF4);
        reg.set("minecraft:warped_forest",                    0x7BB6DA);
        reg.set("minecraft:wooded_badlands_plateau",          0xCBAA3D);
        reg.set("minecraft:wooded_hills",                     0x3BAC30);
        reg.set("minecraft:wooded_mountains",                 0x97B994);
    }

    private void assignColor(String biomeId, int color, ColorSetter setter, Registry<Biome> registry)
    {
        try
        {
            Optional<Biome> optional = registry.getOrEmpty(new Identifier(biomeId));

            if (optional.isPresent())
            {
                int id = this.biomeMapping.getInt(optional.get());

                if (id >= 0)
                {
                    setter.set(id, Color4f.fromColor(color, 0.25f));
                }
            }
        }
        catch (Exception ignore) {}
    }

    private interface ColorRegistry
    {
        void set(String biomeId, int color);
    }

    private interface ColorSetter
    {
        void set(int biomeId, Color4f color);
    }

    private Color4f getColor(int biomeId)
    {
        return this.colorRetriever.apply(biomeId);
    }

    public static int getColorFromHue(int hue)
    {
        return 0xFF000000 | (java.awt.Color.HSBtoRGB((float) (hue % 360) / 360f, 1f, 1f) & 0x00FFFFFF);
    }

    private static BiomeSource getBiomeSourceForChunk(WorldChunk chunk)
    {
        final BiomeAccess biomeAccess = chunk.getWorld().getBiomeAccess();
        BlockPos.Mutable mutablePos = new BlockPos.Mutable();
        BiomeSource biomeSource = (x, y, z) -> biomeAccess.getBiome(mutablePos.set(x, y, z)).value();
        //BiomeSource biomeSource = chunk::getBiomeForNoiseGen;
        //long biomeSeed = ((IMixinBiomeAccess) biomeAccess).minihud_getSeed();
        //BiomeSource biomeSource = (x, y, z) -> MiscUtils.getBiomeMasaOptimization(x, y, z, chunk, biomeSeed);

        return biomeSource;
    }

    private interface BiomeSource
    {
        Biome getBiome(int x, int y, int z);
    }

    protected record ColoredQuad(Vec3i start, int width, int height, Direction side, int biomeId)
    {
        @Override
        public String toString()
        {
            return "ColoredQuad{start=" + this.start + ", width=" + this.width + ", height=" + this.height +
                           ", side=" + this.side + ", biomeId=" + this.biomeId + '}';
        }
    }

    protected static class EdgeStrip
    {
        public final long startPosLong;
        public final Direction side;
        public final int length;
        public final int biomeId;

        public EdgeStrip(long startPosLong, Direction side, int length, int biomeId)
        {
            this.startPosLong = startPosLong;
            this.side = side;
            this.length = length;
            this.biomeId = biomeId;
        }

        @Override
        public boolean equals(Object o)
        {
            if (this == o) {return true;}
            if (o == null || this.getClass() != o.getClass()) {return false;}

            EdgeStrip edgeStrip = (EdgeStrip) o;

            if (this.startPosLong != edgeStrip.startPosLong) {return false;}
            return this.side == edgeStrip.side;
        }

        @Override
        public int hashCode()
        {
            int result = (int) (this.startPosLong ^ (this.startPosLong >>> 32));
            result = 31 * result + this.side.hashCode();
            return result;
        }

        @Override
        public String toString()
        {
            return String.format("EdgeStrip{startPosLong=%08X, side=%s, length=%d, biomeId=%d}",
                                 this.startPosLong, this.side, this.length, this.biomeId);
        }
    }
}
