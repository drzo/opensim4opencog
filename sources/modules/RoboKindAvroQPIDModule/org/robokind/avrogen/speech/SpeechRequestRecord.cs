// ------------------------------------------------------------------------------
// <auto-generated>
//    Generated by RoboKindChat.vshost.exe, version 0.9.0.0
//    Changes to this file may cause incorrect behavior and will be lost if code
//    is regenerated
// </auto-generated>
// ------------------------------------------------------------------------------
namespace org.robokind.avrogen.speech
{
	using System;
	using System.Collections.Generic;
	using System.Text;
	using Avro;
	using Avro.Specific;
	
	public partial class SpeechRequestRecord : ISpecificRecord, SpeechRequest
	{
		private static Schema _SCHEMA = Avro.Schema.Parse(@"{""type"":""record"",""name"":""SpeechRequestRecord"",""namespace"":""org.robokind.avrogen.speech"",""fields"":[{""name"":""speechServiceId"",""type"":""string""},{""name"":""requestSourceId"",""type"":""string""},{""name"":""timestampMillisecUTC"",""type"":""long""},{""name"":""phrase"",""type"":""string""}]}");
		private string _speechServiceId;
		private string _requestSourceId;
		private long _timestampMillisecUTC;
		private string _phrase;
		public virtual Schema Schema
		{
			get
			{
				return SpeechRequestRecord._SCHEMA;
			}
		}
		public string speechServiceId
		{
			get
			{
				return this._speechServiceId;
			}
			set
			{
				this._speechServiceId = value;
			}
		}
		public string requestSourceId
		{
			get
			{
				return this._requestSourceId;
			}
			set
			{
				this._requestSourceId = value;
			}
		}
		public long timestampMillisecUTC
		{
			get
			{
				return this._timestampMillisecUTC;
			}
			set
			{
				this._timestampMillisecUTC = value;
			}
		}
		public string phrase
		{
			get
			{
				return this._phrase;
			}
			set
			{
				this._phrase = value;
			}
		}
		public virtual object Get(int fieldPos)
		{
			switch (fieldPos)
			{
			case 0: return this.speechServiceId;
			case 1: return this.requestSourceId;
			case 2: return this.timestampMillisecUTC;
			case 3: return this.phrase;
			default: throw new AvroRuntimeException("Bad index " + fieldPos + " in Get()");
			};
		}
		public virtual void Put(int fieldPos, object fieldValue)
		{
			switch (fieldPos)
			{
			case 0: this.speechServiceId = (System.String)fieldValue; break;
			case 1: this.requestSourceId = (System.String)fieldValue; break;
			case 2: this.timestampMillisecUTC = (System.Int64)fieldValue; break;
			case 3: this.phrase = (System.String)fieldValue; break;
			default: throw new AvroRuntimeException("Bad index " + fieldPos + " in Put()");
			};
		}
	}
}
