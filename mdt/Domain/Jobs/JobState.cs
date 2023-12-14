using System;

public enum JobStateEnum
{
    PENDING
}

public static class JobState
{
    public static string ToString(JobStateEnum state)
    {
        switch (state)
        {
            case JobStateEnum.PENDING:
                return "Pending";
            default:
                return "Unknown";
        }
    }

    public static JobStateEnum FromCode(int code)
    {
        switch (code)
        {
            case 0:
                return JobStateEnum.PENDING;
            default:
                throw new ArgumentException($"Invalid code: {code}", nameof(code));
        }
    }
}