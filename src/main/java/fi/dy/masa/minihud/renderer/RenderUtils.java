package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import fi.dy.masa.malilib.util.Color4f;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.font.FontRenderer;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.Tessellator;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;

public class RenderUtils
{
    public static void renderVerticalWallsOfLinesWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            BlockPos posStart,
            BlockPos posEnd,
            float rangeH,
            float rangeV,
            float lineIntervalH,
            float lineIntervalV,
            Entity entity,
            int color)
    {
        int xMin = Math.min(posStart.getX(), posEnd.getX());
        int zMin = Math.min(posStart.getZ(), posEnd.getZ());
        int xMax = Math.max(posStart.getX(), posEnd.getX()) + 1;
        int zMax = Math.max(posStart.getZ(), posEnd.getZ()) + 1;
        double posX = entity.x;
        double posZ = entity.z;
        float a = ((color >>> 24) & 0xFF) / 255f;
        float r = ((color >>> 16) & 0xFF) / 255f;
        float g = ((color >>>  8) & 0xFF) / 255f;
        float b = ((color       ) & 0xFF) / 255f;

        //double yMin = Math.max(Math.ceil((posY - rangeV) / lineIntervalV) * lineIntervalV,   0);
        //double yMax = Math.min(Math.ceil((posY + rangeV) / lineIntervalV) * lineIntervalV, 256);
        double yMin = posStart.getY();
        double yMax = posEnd.getY();

        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.X, zMin, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);
        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.X, zMax, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);

        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.Z, xMin, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.Z, xMax, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
    }

    public static void renderVerticalWallsOfLinesWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            Direction.Axis axis,
            BlockPos posStart,
            BlockPos posEnd,
            float rangeH,
            float rangeV,
            float lineIntervalH,
            float lineIntervalV,
            Entity entity,
            int color)
    {
        int xMin = Math.min(posStart.getX(), posEnd.getX());
        int zMin = Math.min(posStart.getZ(), posEnd.getZ());
        int xMax = Math.max(posStart.getX(), posEnd.getX());
        int zMax = Math.max(posStart.getZ(), posEnd.getZ());
        double posX = entity.x;
        double posZ = entity.z;
        float a = ((color >>> 24) & 0xFF) / 255f;
        float r = ((color >>> 16) & 0xFF) / 255f;
        float g = ((color >>>  8) & 0xFF) / 255f;
        float b = ((color       ) & 0xFF) / 255f;

        //double yMin = Math.max(Math.ceil((posY - rangeV) / lineIntervalV) * lineIntervalV,   0);
        //double yMax = Math.min(Math.ceil((posY + rangeV) / lineIntervalV) * lineIntervalV, 256);
        double yMin = posStart.getY();
        double yMax = posEnd.getY();

        if (axis == Direction.Axis.Z)
        {
            renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.X, zMin, posZ, posX, yMin, yMax, rangeH,
                    lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);
        }
        else if (axis == Direction.Axis.X)
        {
            renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.Z, xMin, posX, posZ, yMin, yMax, rangeH,
                    lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
        }
    }

    public static void renderVerticalWallsOfLinesIfWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            Direction.Axis axis,
            double edge, double posOnEdgeAxis, double posOnPerpAxis,
            double yMin, double yMax, float rangeH,
            float lineIntervalH, float lineIntervalV,
            int perpendicularMin, int perpendicularMax,
            float r, float g, float b, float a)
    {
        if (Math.abs(posOnEdgeAxis - edge) <= rangeH)
        {
            double hMin = Math.max(Math.ceil((posOnPerpAxis - rangeH) / lineIntervalH) * lineIntervalV, perpendicularMin);
            double hMax = Math.min(Math.ceil((posOnPerpAxis + rangeH) / lineIntervalH) * lineIntervalV, perpendicularMax);
            float quadAlpha = a / 6f;

            switch (axis)
            {
                case X:
                    for (double y = yMin; y <= yMax; y += lineIntervalV)
                    {
                        bufferLines.vertex(hMin, y, edge).color(r, g, b, a).next();
                        bufferLines.vertex(hMax, y, edge).color(r, g, b, a).next();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        bufferLines.vertex(h, yMin, edge).color(r, g, b, a).next();
                        bufferLines.vertex(h, yMax, edge).color(r, g, b, a).next();
                    }

                    bufferQuads.vertex(hMin, yMin, edge).color(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(hMin, yMax, edge).color(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(hMax, yMax, edge).color(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(hMax, yMin, edge).color(r, g, b, quadAlpha).next();

                    break;
                case Z:
                    for (double y = yMin; y <= yMax; y += lineIntervalV)
                    {
                        bufferLines.vertex(edge, y, hMin).color(r, g, b, a).next();
                        bufferLines.vertex(edge, y, hMax).color(r, g, b, a).next();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        bufferLines.vertex(edge, yMin, h).color(r, g, b, a).next();
                        bufferLines.vertex(edge, yMax, h).color(r, g, b, a).next();
                    }

                    bufferQuads.vertex(edge, yMin, hMin).color(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(edge, yMax, hMin).color(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(edge, yMax, hMax).color(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(edge, yMin, hMax).color(r, g, b, quadAlpha).next();
                    break;
                default:
            }
        }
    }

    public static void renderBoxWithEdgesBatched(BufferBuilder bufferQuads, BufferBuilder bufferLines,
            BlockPos posMin, BlockPos posMax, Color4f colorLines, Color4f colorSides)
    {
        final double x1 = posMin.getX();
        final double y1 = posMin.getY();
        final double z1 = posMin.getZ();
        final double x2 = posMax.getX();
        final double y2 = posMax.getY();
        final double z2 = posMax.getZ();

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(x1, y1, z1, x2, y2, z2, colorSides, bufferQuads);
        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(x1, y1, z1, x2, y2, z2, colorLines, bufferLines);
    }

    public static void drawTextPlate(String text, double x, double y, double z, float scale, MinecraftClient mc)
    {
        renderLabel(text, x, y, z, mc.player.yaw, mc.player.pitch, scale, mc);
    }

    public static void renderLabel(String text, double x, double y, double z, float viewerYaw, float viewerPitch, float scale, MinecraftClient mc)
    {
        boolean flag = false; // sneaking
        boolean isThirdPersonFrontal = false;
        FontRenderer fontrenderer = mc.fontRenderer;

        GlStateManager.alphaFunc(GL11.GL_GREATER, 0.1F);
        GlStateManager.pushMatrix();
        GlStateManager.translated(x, y, z);
        GlStateManager.normal3f(0.0F, 1.0F, 0.0F);

        GlStateManager.rotatef(-viewerYaw, 0.0F, 1.0F, 0.0F);
        GlStateManager.rotatef((isThirdPersonFrontal ? -1 : 1) * viewerPitch, 1.0F, 0.0F, 0.0F);

        GlStateManager.scaled(-scale, -scale, scale);
        GlStateManager.disableLighting();
        GlStateManager.depthMask(false);
        GlStateManager.disableDepthTest();

        GlStateManager.enableBlend();
        fi.dy.masa.malilib.render.RenderUtils.setupBlend();
        GlStateManager.disableTexture();

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder vertexbuffer = tessellator.getBufferBuilder();
        int strLenHalved = fontrenderer.getStringWidth(text) / 2;

        vertexbuffer.begin(7, VertexFormats.POSITION_COLOR);
        vertexbuffer.vertex(-strLenHalved - 1, -1, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).next();
        vertexbuffer.vertex(-strLenHalved - 1,  8, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).next();
        vertexbuffer.vertex( strLenHalved + 1,  8, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).next();
        vertexbuffer.vertex( strLenHalved + 1, -1, 0.0D).color(0.0F, 0.0F, 0.0F, 0.25F).next();
        tessellator.draw();

        GlStateManager.enableTexture();

        fontrenderer.draw(text, -strLenHalved, 0, 0x20FFFFFF);
        GlStateManager.enableDepthTest();

        GlStateManager.depthMask(true);
        fontrenderer.draw(text, -strLenHalved, 0, flag ? 0x20FFFFFF : 0xFFFFFFFF);

        GlStateManager.disableBlend();
        GlStateManager.color4f(1f, 1f, 1f, 1f);
        GlStateManager.popMatrix();
    }
}
