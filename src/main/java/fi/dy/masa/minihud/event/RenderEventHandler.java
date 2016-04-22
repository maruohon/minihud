package fi.dy.masa.minihud.event;

import java.util.ArrayList;
import java.util.List;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.entity.Entity;
import net.minecraft.util.BlockPos;
import net.minecraft.util.MathHelper;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.chunk.Chunk;

import net.minecraftforge.client.event.RenderGameOverlayEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent.ElementType;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

import fi.dy.masa.minihud.config.Configs;

public class RenderEventHandler
{
    public static final int MASK_COORDINATES    = 0x01;
    public static final int MASK_YAW            = 0x02;
    public static final int MASK_PITCH          = 0x04;
    public static final int MASK_SPEED          = 0x08;
    public static final int MASK_BIOME          = 0x10;
    public static final int MASK_LIGHT          = 0x20;

    private final Minecraft mc;
    public static boolean enabled;
    public static int mask;

    public RenderEventHandler()
    {
        this.mc = Minecraft.getMinecraft();
    }

    @SubscribeEvent
    public void onRenderGameOverlay(RenderGameOverlayEvent.Post event)
    {
        if (enabled == false || event.type != ElementType.ALL || this.mc.gameSettings.showDebugInfo == true)
        {
            return;
        }

        Entity entity = this.mc.getRenderViewEntity();
        List<String> lines = new ArrayList<String>();

        if ((mask & MASK_COORDINATES) != 0)
        {
            lines.add(String.format("XYZ: %.4f / %.4f / %.4f", entity.posX, entity.getEntityBoundingBox().minY, entity.posZ));
        }

        int yawPitchSpeed = mask & (MASK_PITCH | MASK_YAW | MASK_SPEED);

        if (yawPitchSpeed != 0)
        {
            String pre = "";
            StringBuilder str = new StringBuilder(64);

            if ((yawPitchSpeed & MASK_YAW) != 0)
            {
                str.append(String.format("%syaw: %.1f", pre, MathHelper.wrapAngleTo180_float(entity.rotationYaw)));
                pre = " / ";
            }

            if ((yawPitchSpeed & MASK_PITCH) != 0)
            {
                str.append(String.format("%spitch: %.1f", pre, MathHelper.wrapAngleTo180_float(entity.rotationPitch)));
                pre = " / ";
            }

            if ((yawPitchSpeed & MASK_SPEED) != 0)
            {
                double dx = entity.posX - entity.lastTickPosX;
                double dy = entity.posY - entity.lastTickPosY;
                double dz = entity.posZ - entity.lastTickPosZ;
                double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                str.append(String.format("%sspeed: %.3f m/s", pre, dist * 20));
                pre = " / ";
            }

            lines.add(str.toString());
        }

        if ((mask & (MASK_BIOME | MASK_LIGHT)) != 0)
        {
            BlockPos pos = new BlockPos(entity.posX, entity.getEntityBoundingBox().minY, entity.posZ);

            if (this.mc.theWorld.isBlockLoaded(pos) == true)
            {
                Chunk chunk = this.mc.theWorld.getChunkFromBlockCoords(pos);

                if (chunk.isEmpty() == false)
                {
                    if ((mask & MASK_BIOME) != 0)
                    {
                        lines.add("Biome: " + chunk.getBiome(pos, this.mc.theWorld.getWorldChunkManager()).biomeName);
                    }

                    if ((mask & MASK_LIGHT) != 0)
                    {
                        lines.add("Light: " + chunk.getLightSubtracted(pos, 0) + " (" + chunk.getLightFor(EnumSkyBlock.SKY, pos) + " sky, " + chunk.getLightFor(EnumSkyBlock.BLOCK, pos) + " block)");
                    }
                }
            }
        }

        int xOff = 4;
        int yOff = 4;

        GlStateManager.pushMatrix();

        if (Configs.useScaledFont == true)
        {
            GlStateManager.scale(0.5, 0.5, 0.5);
        }

        FontRenderer fontRenderer = this.mc.fontRendererObj;

        for (String line : lines)
        {
            if (Configs.useTextBackground == true)
            {
                Gui.drawRect(xOff - 2, yOff - 2, xOff + fontRenderer.getStringWidth(line) + 2, yOff + fontRenderer.FONT_HEIGHT, Configs.textBackgroundColor);
            }

            if (Configs.useFontShadow == true)
            {
                this.mc.ingameGUI.drawString(fontRenderer, line, xOff, yOff, Configs.fontColor);
            }
            else
            {
                fontRenderer.drawString(line, xOff, yOff, Configs.fontColor);
            }

            yOff += fontRenderer.FONT_HEIGHT + 2;
        }

        GlStateManager.popMatrix();
    }
}
