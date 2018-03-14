package fi.dy.masa.minihud.util;

import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.multiplayer.PlayerControllerMP;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.network.play.client.CPacketPlayer;
import net.minecraft.util.EnumActionResult;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.EnumFacing.AxisDirection;
import net.minecraft.util.EnumHand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

public class Tweaks
{
    public static EnumActionResult onProcessRightClickBlock(
            PlayerControllerMP controller,
            EntityPlayerSP player,
            WorldClient world,
            BlockPos pos,
            EnumFacing side,
            Vec3d hitVec,
            EnumHand hand)
    {
        // Alt: Place the block facing/against the adjacent block (= just rotated from normal)
        if (GuiScreen.isAltKeyDown())
        {
            BlockPos posNew = pos.offset(side);
            HitPart hitPart = getHitPart(side, pos, hitVec);
            EnumFacing sideRotated = getRotatedFacing(side, hitPart);
            pos = posNew;

            // Alt + Ctrl: Place the block into the adjacent space
            if (GuiScreen.isCtrlKeyDown())
            {
                pos = pos.offset(sideRotated.getOpposite());
            }
            else
            {
                side = sideRotated;
            }

            boolean rotated = false;

            if (hitPart == HitPart.CENTER)
            {
                player.connection.sendPacket(new CPacketPlayer.Rotation(player.rotationYaw - 180f, player.rotationPitch, player.onGround));
                rotated = true;
            }
            else if (hitPart == HitPart.LEFT)
            {
                player.connection.sendPacket(new CPacketPlayer.Rotation(player.rotationYaw - 90f, player.rotationPitch, player.onGround));
                rotated = true;
            }
            else if (hitPart == HitPart.RIGHT)
            {
                player.connection.sendPacket(new CPacketPlayer.Rotation(player.rotationYaw + 90f, player.rotationPitch, player.onGround));
                rotated = true;
            }

            EnumActionResult result = controller.processRightClickBlock(player, world, pos, side, hitVec, hand);

            if (rotated)
            {
                player.connection.sendPacket(new CPacketPlayer.Rotation(player.rotationYaw, player.rotationPitch, player.onGround));
            }

            return result;
        }

        return controller.processRightClickBlock(player, world, pos, side, hitVec, hand);
    }

    public static EnumFacing getRotatedFacing(EnumFacing originalSide, HitPart hitPart)
    {
        if (originalSide.getAxis().isVertical())
        {
            switch (hitPart)
            {
                case LEFT:      return EnumFacing.EAST;
                case RIGHT:     return EnumFacing.WEST;
                case BOTTOM:    return EnumFacing.NORTH;
                case TOP:       return EnumFacing.SOUTH;
                case CENTER:    return originalSide.getOpposite();
                default:        return originalSide;
            }
        }
        else
        {
            switch (hitPart)
            {
                case LEFT:      return originalSide.rotateYCCW();
                case RIGHT:     return originalSide.rotateY();
                case BOTTOM:    return EnumFacing.UP;
                case TOP:       return EnumFacing.DOWN;
                case CENTER:    return originalSide.getOpposite();
                default:        return originalSide;
            }
        }
    }

    public static HitPart getHitPart(EnumFacing originalSide, BlockPos pos, Vec3d hitVec)
    {
        double x = hitVec.x - pos.getX();
        double y = hitVec.y - pos.getY();
        double z = hitVec.z - pos.getZ();
        double posH = 0;
        double posV = 0;

        switch (originalSide)
        {
            case DOWN:
                posH = x;
                posV = z;
                break;
            case UP:
                posH = x;
                posV = 1.0d - z;
                break;
            case NORTH:
            case SOUTH:
                posH = originalSide.getAxisDirection() == AxisDirection.POSITIVE ? x : 1.0d - x;
                posV = y;
                break;
            case WEST:
            case EAST:
                posH = originalSide.getAxisDirection() == AxisDirection.NEGATIVE ? z : 1.0d - z;
                posV = y;
                break;
        }

        double offH = Math.abs(posH - 0.5d);
        double offV = Math.abs(posV - 0.5d);

        if (offH > 0.25d || offV > 0.25d)
        {
            if (offH > offV)
            {
                return posH < 0.5d ? HitPart.LEFT : HitPart.RIGHT;
            }
            else
            {
                return posV < 0.5d ? HitPart.BOTTOM : HitPart.TOP;
            }
        }
        else
        {
            return HitPart.CENTER;
        }
    }

    public enum HitPart
    {
        CENTER,
        LEFT,
        RIGHT,
        BOTTOM,
        TOP;
    }
}
