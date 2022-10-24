package fi.dy.masa.minihud.network.servux;

import java.util.List;
import com.google.common.collect.ImmutableList;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;

import malilib.network.message.BasePacketHandler;
import malilib.overlay.message.MessageDispatcher;
import fi.dy.masa.minihud.MiniHUD;

public class ServuxInfoSubRegistrationPacketHandler extends BasePacketHandler
{
    public static final ServuxInfoSubRegistrationPacketHandler INSTANCE = new ServuxInfoSubRegistrationPacketHandler();
    public static final ResourceLocation REG_CHANNEL = new ResourceLocation("servux:info_reg");

    protected static final List<ResourceLocation> CHANNELS = ImmutableList.of(REG_CHANNEL);
    protected static final int PACKET_S2C_METADATA = 1;

    private ServuxInfoSubRegistrationPacketHandler()
    {
        this.registerToServer = true;
    }

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        int type = buf.readVarInt();

        MiniHUD.debugLog("ServuxInfoSubRegistrationPacketHandler#onPacketReceived() - type = {}", type);

        if (type == PACKET_S2C_METADATA)
        {
            try
            {
                NBTTagCompound tag = buf.readCompoundTag();
                ServuxInfoSubDataPacketHandler.INSTANCE.receiveMetadata(tag);
                MiniHUD.debugLog("ServuxInfoSubRegistrationPacketHandler#onPacketReceived() - tag: '{}'", tag);
            }
            catch (Exception e)
            {
                MessageDispatcher.error().console(e).translate("minihud.message.error.info_sub.failed_receive_metadata");
            }
        }
    }
}
