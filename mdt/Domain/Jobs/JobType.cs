using System;

public enum JobTypeEnum
{
    SURVEILLANCE,
    DELIVERY
}

public static class JobType
{
    public static string ToString(JobTypeEnum jobType)
    {
        switch (jobType)
        {
            case JobTypeEnum.SURVEILLANCE:
                return "Surveillance";
            case JobTypeEnum.DELIVERY:
                return "Delivery";
            default:
                return "Unknown";
        }
    }

    public static int ToCode(JobTypeEnum jobType)
    {
        switch (jobType)
        {
            case JobTypeEnum.SURVEILLANCE:
                return 0;
            case JobTypeEnum.DELIVERY:
                return 1;
            default:
                throw new ArgumentException($"Invalid job type: {jobType}", nameof(jobType));
        }
    }

    public static JobTypeEnum FromCode(int code)
    {
        switch (code)
        {
            case 0:
                return JobTypeEnum.SURVEILLANCE;
            case 1:
                return JobTypeEnum.DELIVERY;
            default:
                throw new ArgumentException($"Invalid code: {code}", nameof(code));
        }
    }
}
