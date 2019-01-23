package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;

public class OverlayRendererSlimeChunks extends OverlayRendererBase
{
    public static double overlayTopY;

    protected final BlockPos.Mutable posMutable1 = new BlockPos.Mutable();
    protected final BlockPos.Mutable posMutable2 = new BlockPos.Mutable();
    protected double topY;

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() && mc.world.dimension.hasVisibleSky();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        if (this.topY != overlayTopY)
        {
            return true;
        }

        int ex = (int) Math.floor(entity.x);
        int ez = (int) Math.floor(entity.z);
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        return Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Entity entity, MinecraftClient mc)
    {
        DataStorage data = DataStorage.getInstance();
        this.topY = overlayTopY;

        if (data.isWorldSeedKnown(entity.dimension))
        {
            final int centerX = ((int) MathHelper.floor(entity.x)) >> 4;
            final int centerZ = ((int) MathHelper.floor(entity.z)) >> 4;
            final long worldSeed = data.getWorldSeed(entity.dimension);
            final Color4f colorLines = Configs.Colors.SLIME_CHUNKS_OVERLAY_COLOR.getColor();
            final Color4f colorSides = Color4f.fromColor(colorLines, colorLines.a / 6);
            int r = MathHelper.clamp(Configs.Generic.SLIME_CHUNK_OVERLAY_RADIUS.getIntegerValue(), -1, 40);

            if (r == -1)
            {
                r = mc.options.viewDistance;
            }

            RenderObjectBase renderQuads = this.renderObjects.get(0);
            RenderObjectBase renderLines = this.renderObjects.get(1);
            BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
            BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

            for (int xOff = -r; xOff <= r; xOff++)
            {
                for (int zOff = -r; zOff <= r; zOff++)
                {
                    int cx = centerX + xOff;
                    int cz = centerZ + zOff;

                    if (MiscUtils.canSlimeSpawnInChunk(cx, cz, worldSeed))
                    {
                        this.posMutable1.set( cx << 4,               0,  cz << 4);
                        this.posMutable2.set((cx << 4) + 16, this.topY, (cz << 4) + 16);
                        RenderUtils.renderBoxWithEdgesBatched(BUFFER_1, BUFFER_2, this.posMutable1, this.posMutable2, colorLines, colorSides);
                    }
                }
            }

            BUFFER_1.end();
            BUFFER_2.end();

            renderQuads.uploadData(BUFFER_1);
            renderLines.uploadData(BUFFER_2);

            this.lastUpdatePos = new BlockPos(entity);
        }
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }
}
