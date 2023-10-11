package fi.dy.masa.minihud.config;

import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.network.ClientPacketChannelHandler;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.network.StructurePacketHandlerCarpet;
import fi.dy.masa.minihud.network.StructurePacketHandlerServux;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;
import fi.dy.masa.minihud.renderer.OverlayRendererBiomeBorders;
import fi.dy.masa.minihud.renderer.OverlayRendererConduitRange;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererRandomTickableChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererRegion;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnChunks;
import fi.dy.masa.minihud.util.DataStorage;

public class RendererCallbacks
{
    public static void onBeaconRangeToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererBeaconRange.INSTANCE.setNeedsUpdate();
        }
    }

    public static void onBiomeBorderToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererBiomeBorders.INSTANCE.setNeedsUpdate();
        }
    }

    public static void onConduitRangeToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererConduitRange.INSTANCE.setNeedsUpdate();
        }
    }

    public static void onLightLevelToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererLightLevel.setNeedsUpdate();
        }
    }

    public static void onRandomTicksFixedToggled(IConfigBoolean config)
    {
        Entity entity = EntityUtils.getCameraEntity();

        if (config.getBooleanValue() && entity != null)
        {
            Vec3d pos = entity.getPos();
            OverlayRendererRandomTickableChunks.newPos = pos;
            String green = GuiBase.TXT_GREEN;
            String rst = GuiBase.TXT_RST;
            String strStatus = green + StringUtils.translate("malilib.message.value.on") + rst;
            String strPos = String.format("x: %.2f, y: %.2f, z: %.2f", pos.x, pos.y, pos.z);
            String message = StringUtils.translate("minihud.message.toggled_using_position", config.getPrettyName(), strStatus, strPos);

            InfoUtils.printActionbarMessage(message);
        }
    }

    public static void onRandomTicksPlayerToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererRandomTickableChunks.setNeedsUpdate();
        }
    }

    public static void onRegionFileToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererRegion.setNeedsUpdate();
        }
    }

    public static void onSlimeChunksToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererSlimeChunks.setNeedsUpdate();
            OverlayRendererSlimeChunks.onEnabled();
        }
    }

    public static void onSpawnChunksPlayerToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererSpawnChunks.setNeedsUpdate();
        }
    }

    public static void onSpawnChunksRealToggled(IConfigBoolean config)
    {
        if (config.getBooleanValue())
        {
            OverlayRendererSpawnChunks.setNeedsUpdate();

            BlockPos spawn = DataStorage.getInstance().getWorldSpawn();
            String green = GuiBase.TXT_GREEN;
            String rst = GuiBase.TXT_RST;
            String strStatus = green + StringUtils.translate("malilib.message.value.on") + rst;
            String strPos = String.format("x: %d, y: %d, z: %d", spawn.getX(), spawn.getY(), spawn.getZ());
            String message = StringUtils.translate("minihud.message.toggled_using_world_spawn", config.getPrettyName(), strStatus, strPos);

            InfoUtils.printActionbarMessage(message);
        }
    }

    public static void onStructuresToggled(IConfigBoolean config)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (mc != null && mc.player != null)
        {
            if (mc.isIntegratedServerRunning() == false)
            {
                if (config.getBooleanValue())
                {
                    DataStorage.getInstance().registerStructureChannel();
                }
                else
                {
                    ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandlerCarpet.INSTANCE);
                    ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandlerServux.INSTANCE);
                }
            }
            else
            {
                DataStorage.getInstance().setStructuresNeedUpdating();
            }
        }
    }
}
