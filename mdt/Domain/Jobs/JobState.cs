using System;
using System.Globalization;

namespace DDDSample1.Domain.Jobs
{

    public enum JobStateEnum
    {
        PENDING,
        APPROVED,
        REJECTED,
        PLANNED,
    }

    public class JobState
    {
        public static string ToString(JobStateEnum state)
        {
            return state switch
            {
                JobStateEnum.PENDING => "Pending",
                JobStateEnum.APPROVED => "Approved",
                JobStateEnum.REJECTED => "Rejected",
                JobStateEnum.PLANNED => "Planned",
                _ => "Unknown"
            };
        }

        public static JobStateEnum FromString(string state)
        {
            return state.ToUpper(CultureInfo.InvariantCulture) switch
            {
                "PENDING" => JobStateEnum.PENDING,
                "APPROVED" => JobStateEnum.APPROVED,
                "REJECTED" => JobStateEnum.REJECTED,
                "PLANNED" => JobStateEnum.PLANNED,
                _ => throw new ArgumentException($"Bad job state: {state}")
            };
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
}
