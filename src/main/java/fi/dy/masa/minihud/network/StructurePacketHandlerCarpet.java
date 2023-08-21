package fi.dy.masa.minihud.network;

import net.fabricmc.fabric.api.networking.v1.PacketSender;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.network.ClientPlayNetworkHandler;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtList;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.util.Identifier;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.util.DataStorage;

public class StructurePacketHandlerCarpet {
    public static final Identifier CHANNEL = new Identifier("carpet:structures");
    public static final int PACKET_S2C_DATA = 0;
    public static final int VERSION = 1;

    public static final StructurePacketHandlerCarpet INSTANCE = new StructurePacketHandlerCarpet();

    private boolean registered;
    private int timeout;

    public void reset()
    {
        this.registered = false;
    }

    public void onPacketReceived(MinecraftClient client, ClientPlayNetworkHandler handler, PacketByteBuf buf, PacketSender sender)
    {
        int id = buf.readVarInt();

        MiniHUD.printDebug("StructurePacketHandlerCarpet#onPacketReceived(): " + id);

        if (id == PACKET_S2C_DATA)
        {
            NbtCompound tag = buf.readNbt();

            if (tag != null)
            {
                // Normal structure data packet
                if (this.registered && tag.contains("Structures", Constants.NBT.TAG_LIST))
                {
                    NbtList structures = tag.getList("Structures", Constants.NBT.TAG_COMPOUND);
                    DataStorage.getInstance().addOrUpdateStructuresFromServer(structures, this.timeout, false);
                }
                // Metadata packet upon channel registration
                else if (tag.contains("Version", Constants.NBT.TAG_INT) &&
                         tag.contains("Timeout", Constants.NBT.TAG_INT) &&
                         tag.getInt("Version") == VERSION)
                {
                    this.timeout = tag.getInt("Timeout");
                    this.registered = true;
                }
            }
        }
    }
}
