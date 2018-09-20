package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;

public class OverlayRendererSpawnableColumnHeights extends OverlayRendererBase
{
    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_SPAWNABLE_COLUMN_HEIGHTS.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        int ex = (int) Math.floor(entity.posX);
        int ez = (int) Math.floor(entity.posZ);
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();
        return Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        final int color = Configs.Colors.SPAWNABLE_COLUMNS_OVERLAY_COLOR.getIntegerValue();
        final int radius = MathHelper.clamp(Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue(), 0, 128);

        final float a = ((color >>> 24) & 0xFF) / 255f;
        final float r = ((color >>> 16) & 0xFF) / 255f;
        final float g = ((color >>>  8) & 0xFF) / 255f;
        final float b = ((color       ) & 0xFF) / 255f;

        final int xStart = (int) entity.posX - radius;
        final int zStart = (int) entity.posZ - radius;
        final int xEnd = (int) entity.posX + radius;
        final int zEnd = (int) entity.posZ + radius;
        final WorldClient world = mc.world;

        BUFFER_1.begin(GL11.GL_TRIANGLE_STRIP, DefaultVertexFormats.POSITION_COLOR);

        for (int x = xStart; x <= xEnd; ++x)
        {
            for (int z = zStart; z <= zEnd; ++z)
            {
                // See WorldEntitySpawner.getRandomChunkPosition()
                int height = MathHelper.roundUp(world.getHeight(x, z) + 1, 16);

                if (height == 0)
                {
                    height = world.getChunkFromChunkCoords(x << 4, z << 4).getTopFilledSegment() + 15;
                }

                final double boxX = x;
                final double boxZ = z;
                final double boxY = height;

                RenderUtils.addChainedSidesAndTopBoxVertices(BUFFER_1, r, g, b, a,
                        boxX + 0.25F, boxY           , boxZ + 0.25F,
                        boxX + 0.75F, boxY + 0.09375D, boxZ + 0.75F);
            }
        }

        BUFFER_1.finishDrawing();

        this.renderObjects.get(0).uploadData(BUFFER_1);

        this.lastUpdatePos = new BlockPos(entity.posX, 0, entity.posZ);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_TRIANGLE_STRIP);
    }
}
