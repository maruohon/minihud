package fi.dy.masa.minihud.event;

import org.lwjgl.opengl.GL11;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import fi.dy.masa.minihud.config.Configs;

public class OverlayRenderer
{
    public static void renderOverlays(long mask, Entity entity, float partialTicks)
    {
        GlStateManager.depthMask(false);
        GlStateManager.disableLighting();
        GlStateManager.disableCull();
        GlStateManager.enableBlend();
        GlStateManager.pushMatrix();
        GlStateManager.disableTexture2D();

        if ((mask & RenderEventHandler.MASK_REGION_OVERLAY) != 0)
        {
            int rx = MathHelper.floor(entity.posX) & ~0x1FF;
            int rz = MathHelper.floor(entity.posZ) & ~0x1FF;
            BlockPos pos1 = new BlockPos(rx,         0, rz      );
            BlockPos pos2 = new BlockPos(rx + 511, 256, rz + 511);
            Minecraft mc = Minecraft.getMinecraft();
            int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
            int color = Configs.regionOverlayColor;
            float a = ((color >>> 24) & 0xFF) / 255f;
            float r = ((color >>> 16) & 0xFF) / 255f;
            float g = ((color >>>  8) & 0xFF) / 255f;
            float b = ((color       ) & 0xFF) / 255f;

            GlStateManager.glLineWidth(1.6f);

            renderVerticalWallsOfLinesWithinRange(pos1, pos2, rangeH, 256, 16, 16, entity, r, g, b, a, partialTicks);
        }

        GlStateManager.popMatrix();
        GlStateManager.enableTexture2D();
        GlStateManager.disableBlend();
        GlStateManager.enableCull();
        GlStateManager.depthMask(true);
    }

    private static void renderVerticalWallsOfLinesWithinRange(
            BlockPos posStart,
            BlockPos posEnd,
            float rangeH,
            float rangeV,
            float lineIntervalH,
            float lineIntervalV,
            Entity entity,
            float r, float g, float b, float a,
            float partialTicks)
    {
        int xMin = Math.min(posStart.getX(), posEnd.getX());
        int zMin = Math.min(posStart.getZ(), posEnd.getZ());
        int xMax = Math.max(posStart.getX(), posEnd.getX()) + 1;
        int zMax = Math.max(posStart.getZ(), posEnd.getZ()) + 1;
        double posX = entity.posX;
        double posZ = entity.posZ;
        double dx = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
        double dy = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
        double dz = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;

        //double yMin = Math.max(Math.ceil((posY - rangeV) / lineIntervalV) * lineIntervalV,   0);
        //double yMax = Math.min(Math.ceil((posY + rangeV) / lineIntervalV) * lineIntervalV, 256);
        double yMin =   0;
        double yMax = 256;

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder buffer = tessellator.getBuffer();

        renderVerticalWallsOfLinesIfWithinRange(buffer, EnumFacing.Axis.X, zMin, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, dx, dy, dz, r, g, b, a, partialTicks);
        renderVerticalWallsOfLinesIfWithinRange(buffer, EnumFacing.Axis.X, zMax, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, dx, dy, dz, r, g, b, a, partialTicks);

        renderVerticalWallsOfLinesIfWithinRange(buffer, EnumFacing.Axis.Z, xMin, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, dx, dy, dz, r, g, b, a, partialTicks);
        renderVerticalWallsOfLinesIfWithinRange(buffer, EnumFacing.Axis.Z, xMax, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, dx, dy, dz, r, g, b, a, partialTicks);
    }

    private static void renderVerticalWallsOfLinesIfWithinRange(
            BufferBuilder buffer,
            EnumFacing.Axis axis,
            double edge, double posOnEdgeAxis, double posOnPerpAxis,
            double yMin, double yMax, float rangeH,
            float lineIntervalH, float lineIntervalV,
            int perpendicularMin, int perpendicularMax,
            double dx, double dy, double dz,
            float r, float g, float b, float a,
            float partialTicks)
    {
        if (Math.abs(posOnEdgeAxis - edge) <= rangeH)
        {
            Tessellator tessellator = Tessellator.getInstance();
            double hMin = Math.max(Math.ceil((posOnPerpAxis - rangeH) / lineIntervalH) * lineIntervalV, perpendicularMin);
            double hMax = Math.min(Math.ceil((posOnPerpAxis + rangeH) / lineIntervalH) * lineIntervalV, perpendicularMax);
            float quadAlpha = a / 6f;

            switch (axis)
            {
                case X:
                    buffer.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION_COLOR);

                    for (double y = yMin; y <= yMax; y += lineIntervalV)
                    {
                        buffer.pos(hMin - dx, y - dy, edge - dz).color(r, g, b, a).endVertex();
                        buffer.pos(hMax - dx, y - dy, edge - dz).color(r, g, b, a).endVertex();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        buffer.pos(h - dx, yMin - dy, edge - dz).color(r, g, b, a).endVertex();
                        buffer.pos(h - dx, yMax - dy, edge - dz).color(r, g, b, a).endVertex();
                    }

                    tessellator.draw();

                    buffer.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_COLOR);
                    buffer.pos(hMin - dx, yMin - dy, edge - dz).color(r, g, b, quadAlpha).endVertex();
                    buffer.pos(hMin - dx, yMax - dy, edge - dz).color(r, g, b, quadAlpha).endVertex();
                    buffer.pos(hMax - dx, yMax - dy, edge - dz).color(r, g, b, quadAlpha).endVertex();
                    buffer.pos(hMax - dx, yMin - dy, edge - dz).color(r, g, b, quadAlpha).endVertex();
                    tessellator.draw();

                    break;
                case Z:
                    buffer.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION_COLOR);

                    for (double y = yMin; y <= yMax; y += lineIntervalV)
                    {
                        buffer.pos(edge - dx, y - dy, hMin - dz).color(r, g, b, a).endVertex();
                        buffer.pos(edge - dx, y - dy, hMax - dz).color(r, g, b, a).endVertex();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        buffer.pos(edge - dx, yMin - dy, h - dz).color(r, g, b, a).endVertex();
                        buffer.pos(edge - dx, yMax - dy, h - dz).color(r, g, b, a).endVertex();
                    }

                    tessellator.draw();

                    buffer.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_COLOR);
                    buffer.pos(edge - dx, yMin - dy, hMin - dz).color(r, g, b, quadAlpha).endVertex();
                    buffer.pos(edge - dx, yMax - dy, hMin - dz).color(r, g, b, quadAlpha).endVertex();
                    buffer.pos(edge - dx, yMax - dy, hMax - dz).color(r, g, b, quadAlpha).endVertex();
                    buffer.pos(edge - dx, yMin - dy, hMax - dz).color(r, g, b, quadAlpha).endVertex();
                    tessellator.draw();
                    break;
                default:
            }
        }
    }

    /*
    private static void renderBoxOutline(BlockPos posStart, BlockPos posEnd, Entity entity, float partialTicks, float r, float g, float b)
    {
        AxisAlignedBB aabb = createEnclosingAABB(posStart, posEnd, entity, partialTicks);
        RenderGlobal.drawSelectionBoundingBox(aabb, r, g, b, 0xCC / 255f);
    }

    private static AxisAlignedBB createEnclosingAABB(BlockPos posStart, BlockPos posEnd, Entity entity, float partialTicks)
    {
        int minX = Math.min(posStart.getX(), posEnd.getX());
        int minY = Math.min(posStart.getY(), posEnd.getY());
        int minZ = Math.min(posStart.getZ(), posEnd.getZ());
        int maxX = Math.max(posStart.getX(), posEnd.getX()) + 1;
        int maxY = Math.max(posStart.getY(), posEnd.getY()) + 1;
        int maxZ = Math.max(posStart.getZ(), posEnd.getZ()) + 1;

        return createAABB(minX, minY, minZ, maxX, maxY, maxZ, 0, partialTicks, entity);
    }

    private static AxisAlignedBB createAABB(int x, int y, int z, double expand, double partialTicks, Entity entity)
    {
        return createAABB(x, y, z, x + 1, y + 1, z + 1, expand, partialTicks, entity);
    }

    private static AxisAlignedBB createAABB(int minX, int minY, int minZ, int maxX, int maxY, int maxZ, double expand, double partialTicks, Entity entity)
    {
        double dx = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
        double dy = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
        double dz = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;

        return new AxisAlignedBB(   minX - dx - expand, minY - dy - expand, minZ - dz - expand,
                                    maxX - dx + expand, maxY - dy + expand, maxZ - dz + expand);
    }
    */
}
