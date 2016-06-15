package fi.dy.masa.minihud.event;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.entity.Entity;
import net.minecraft.util.BlockPos;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.MathHelper;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.client.event.RenderGameOverlayEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent.ElementType;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import fi.dy.masa.minihud.config.Configs;

public class RenderEventHandler
{
    public static final int MASK_COORDINATES    = 0x0001;
    public static final int MASK_YAW            = 0x0002;
    public static final int MASK_PITCH          = 0x0004;
    public static final int MASK_SPEED          = 0x0008;
    public static final int MASK_BIOME          = 0x0010;
    public static final int MASK_LIGHT          = 0x0020;
    public static final int MASK_FACING         = 0x0040;
    public static final int MASK_BLOCK          = 0x0080;
    public static final int MASK_CHUNK          = 0x0100;
    public static final int MASK_LOOKINGAT      = 0x0200;
    public static final int MASK_FPS            = 0x0400;
    public static final int MASK_ENTITIES       = 0x0800;

    private static RenderEventHandler instance;
    private final Minecraft mc;
    private boolean enabled;
    private int mask;
    private int fps;
    private int fpsCounter;
    private long fpsUpdateTime = Minecraft.getSystemTime();

    private class StringHolder implements Comparable<StringHolder>
    {
        public final String str;

        public StringHolder(String str)
        {
            this.str = str;
        }

        @Override
        public int compareTo(StringHolder other)
        {
            int lenThis = this.str.length();
            int lenOther = other.str.length();

            if (lenThis == lenOther)
            {
                return 0;
            }

            return this.str.length() > other.str.length() ? -1 : 1;
        }
    }

    public RenderEventHandler()
    {
        this.mc = Minecraft.getMinecraft();
    }

    @SubscribeEvent
    public void onRenderGameOverlay(RenderGameOverlayEvent.Post event)
    {
        if (this.enabled == false || event.type != ElementType.ALL || this.mc.gameSettings.showDebugInfo == true)
        {
            return;
        }

        List<StringHolder> lines = new ArrayList<StringHolder>();

        this.getLines(lines, this.mask);

        if (Configs.sortLinesByLength == true)
        {
            Collections.sort(lines);

            if (Configs.sortLinesReversed == true)
            {
                Collections.reverse(lines);
            }
        }

        this.renderText(Configs.textPosX, Configs.textPosY, lines);
    }

    public static RenderEventHandler getInstance()
    {
        if (instance == null)
        {
            instance = new RenderEventHandler();
        }

        return instance;
    }

    public void setEnabledMask(int mask)
    {
        this.mask = mask;
    }

    public void xorEnabledMask(int mask)
    {
        this.mask ^= mask;
    }

    public void toggleEnabled()
    {
        this.enabled = ! this.enabled;
    }

    private void getLines(List<StringHolder> lines, int enabledMask)
    {
        Entity entity = this.mc.getRenderViewEntity();
        BlockPos pos = new BlockPos(entity.posX, entity.getEntityBoundingBox().minY, entity.posZ);

        this.fpsCounter++;

        while (Minecraft.getSystemTime() >= this.fpsUpdateTime + 1000L)
        {
            this.fps = this.fpsCounter;
            this.fpsUpdateTime += 1000L;
            this.fpsCounter = 0;
        }

        if ((enabledMask & MASK_FPS) != 0)
        {
            lines.add(new StringHolder(String.format("%d fps", this.fps)));
        }

        if ((enabledMask & MASK_COORDINATES) != 0)
        {
            if (Configs.coordinateFormatCustomized == true)
            {
                try
                {
                    lines.add(new StringHolder(String.format(Configs.coordinateFormat,
                        entity.posX, entity.getEntityBoundingBox().minY, entity.posZ)));
                }
                // Uh oh, someone done goofed their format string... :P
                catch (Exception e) { }
            }
            else
            {
                lines.add(new StringHolder(String.format("XYZ: %.4f / %.4f / %.4f",
                    entity.posX, entity.getEntityBoundingBox().minY, entity.posZ)));
            }
        }

        if ((enabledMask & MASK_BLOCK) != 0)
        {
            lines.add(new StringHolder(String.format("Block: %d / %d / %d", pos.getX(), pos.getY(), pos.getZ())));
        }

        if ((enabledMask & MASK_CHUNK) != 0)
        {
            lines.add(new StringHolder(String.format("Block: %d %d %d in Chunk: %d %d %d",
                    pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                    pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4)));
        }

        int yawPitchSpeed = enabledMask & (MASK_PITCH | MASK_YAW | MASK_SPEED);

        if (yawPitchSpeed != 0)
        {
            String pre = "";
            StringBuilder str = new StringBuilder(128);

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

            lines.add(new StringHolder(str.toString()));
        }

        if ((enabledMask & MASK_FACING) != 0)
        {
            EnumFacing facing = entity.getHorizontalFacing();
            String str = "Invalid";

            switch (facing)
            {
                case NORTH: str = "Negative Z"; break;
                case SOUTH: str = "Positive Z"; break;
                case WEST:  str = "Negative X"; break;
                case EAST:  str = "Positive X"; break;
                default:
            }

            lines.add(new StringHolder(String.format("Facing: %s (%s)", facing, str)));
        }

        if ((enabledMask & (MASK_BIOME | MASK_LIGHT)) != 0)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && this.mc.theWorld.isBlockLoaded(pos) == true)
            {
                Chunk chunk = this.mc.theWorld.getChunkFromBlockCoords(pos);

                if (chunk.isEmpty() == false)
                {
                    if ((enabledMask & MASK_BIOME) != 0)
                    {
                        lines.add(new StringHolder("Biome: " + chunk.getBiome(pos, this.mc.theWorld.getWorldChunkManager()).biomeName));
                    }

                    if ((enabledMask & MASK_LIGHT) != 0)
                    {
                        lines.add(new StringHolder("Light: " + chunk.getLightSubtracted(pos, 0) +
                                " (" + chunk.getLightFor(EnumSkyBlock.SKY, pos) + " sky, " +
                                chunk.getLightFor(EnumSkyBlock.BLOCK, pos) + " block)"));
                    }
                }
            }
        }

        if ((enabledMask & MASK_ENTITIES) != 0)
        {
            String ent = this.mc.renderGlobal.getDebugInfoEntities();

            int p = ent.indexOf(",");
            if (p != -1)
            {
                ent = ent.substring(0, p);
            }

            lines.add(new StringHolder(ent));
        }

        if ((enabledMask & MASK_LOOKINGAT) != 0)
        {
            if (this.mc.objectMouseOver != null &&
                this.mc.objectMouseOver.typeOfHit == MovingObjectPosition.MovingObjectType.BLOCK &&
                this.mc.objectMouseOver.getBlockPos() != null)
            {
                BlockPos lookPos = this.mc.objectMouseOver.getBlockPos();
                lines.add(new StringHolder(String.format("Looking at: %d %d %d", lookPos.getX(), lookPos.getY(), lookPos.getZ())));
            }
        }
    }

    private void renderText(int xOff, int yOff, List<StringHolder> lines)
    {
        GlStateManager.pushMatrix();

        if (Configs.useScaledFont == true)
        {
            GlStateManager.scale(0.5, 0.5, 0.5);
        }

        FontRenderer fontRenderer = this.mc.fontRendererObj;

        for (StringHolder holder : lines)
        {
            String line = holder.str;

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
